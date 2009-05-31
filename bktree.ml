(*
The data structure used for the index is a modified bktree.
Each tree node contains a single document and its associated latex strings.
The children of the node are stored according to their distance from the node.

The left_edit_distance satisfies the triangle inequality: for any a,b,c
  left_edit_dist a c <= left_edit_dist a b + left_edit_dist b c
From this we can derive:
  min_i (Query.distance query b_i) >= min_j (Query.distance query c_j) - max_j min_i (left_edit_dist b_i c_j)
Alternatively:
  min_i (Query.distance query b_i) >= max_j (Query.distance query c_j) - min_j min_i (left_edit_dist b_i c_j)
Where i and j range over all the equations in the documents b and c
This is the crucial fact that allows efficient searching within the tree.
*)

type doi = string
type id = string

(* A single document *)
type node =
  { doi : doi
  ; equations : (id * Latex.t) list }

let node_of doi equations =
    { doi = doi
    ; equations = equations }

(* Return each equation whose distance to the query is less than the cutoff, as well as the min/max distance *)
let query_against query equations cutoff =
  List.fold_left
    (fun (min_dist,max_dist,matches) (id,latex) ->
      let dist = Query.distance query latex in
      if dist < cutoff then (min dist min_dist, max dist max_dist, id :: matches) else (min_dist, max_dist, matches))
    (max_int,0,[])
    equations

(* Minimum distance between any equation in the left list and any in the right list
   ie. min_j min_i (left_edit_dist b_i c_j) *)
let index_dist bl cl =
  Util.minimum (List.map (fun (_,b) ->
    Util.minimum (List.map (fun (_,c) ->
      Edit.left_edit_distance b c)
    cl))
  bl)

module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

(* A tree node. The root_deleted field allows deleting documents from the index without rebalancing the tree
   If deletions are more common than expected this may need to be changed *)
type bktree =
  { root : node
  ; mutable root_deleted : bool
  ; mutable children : bktree IntMap.t }

(* A dummy node sits at the top of the tree. *)
let empty =
  { root = node_of "" [("",Latex.empty ())]
  ; root_deleted = true
  ; children = IntMap.empty }

let empty_branch node =
  { root = node
  ; root_deleted = false
  ; children = IntMap.empty }

let rec add node bktree =
  let d = index_dist node.equations bktree.root.equations in
  try
    add node (IntMap.find d bktree.children)
  with Not_found ->
    bktree.children <- IntMap.add d (empty_branch node) bktree.children

let rec delete doi bktree =
  if bktree.root.doi = doi then bktree.root_deleted <- true else ();
  IntMap.iter (fun _ child -> delete doi child) bktree.children

type result = doi * (id list)

(*
The search structure tracks the progress of a search through a tree.
The search will eventually return every document node whose distance to the query is less than the cutoff.

search.unsearched stores tree nodes which have yet to be searched.
Tree nodes in search.unsearched.(i) are guaranteed not to contain any document nodes with distance < i from the query.

search.min_dist tracks the smallest non-empty index in search.unsearches,
so every document node remaining in the search has distance >= min_dist.

search.results stores matches for the query.
search.results.(i) contains matches with dist i from the query.

search.safe_results stores the number of results with distance < min_dist.
no as-yet-unsearched results will match more closely than these
so if the number of results required is less than search.safe_results it is safe to stop searching
*)

type search =
  { query : Query.t
  ; cutoff : int
  ; unsearched : bktree list array
  ; results : result list array
  ; mutable safe_results : int
  ; mutable min_dist : int }

(* Query a document node, update the search structure, return the min/max distance from the query *)
let query_node search node node_deleted =
  let (min_dist,max_dist,matches) = query_against search.query node.equations search.cutoff in
  if min_dist < search.cutoff && not node_deleted
  then search.results.(min_dist) <- (node.doi,matches) :: search.results.(min_dist)
  else ();
  if min_dist < search.min_dist
  then search.safe_results <- search.safe_results + 1
  else ();
  (min_dist,max_dist)

(* Push a tree node onto the search queue *)
let push_search_node search node d =
  if d >= search.cutoff then () else
  search.unsearched.(d) <- node :: search.unsearched.(d)

(* Pop the closest search node from the search queue. *)
let rec pop_search_node search =
  if search.min_dist >= search.cutoff then None else
  match search.unsearched.(search.min_dist) with
    | [] ->
      (* Nothing left at this distance *)
      search.safe_results <- search.safe_results + List.length(search.results.(search.min_dist));
      search.min_dist <- search.min_dist + 1;
      pop_search_node search
    | (bktree::rest) ->
      (* Search in bktree, put the rest back *)
      search.unsearched.(search.min_dist) <- rest;
      Some bktree

(* The initial search structure *)
let new_search query bktree =
  (* The choice of cutoff is completely arbitrary *)
  let cutoff = 1 + (Query.length query / 3)  in
  let search =
    { query = query
    ; cutoff = cutoff
    ; unsearched = Array.make cutoff []
    ; results = Array.make cutoff []
    ; safe_results = 0
    ; min_dist = 0 } in
  IntMap.iter (fun i node -> push_search_node search node (Query.length query - i)) bktree.children;
  search

(* Runs search until at least k results can be returned *)
let run_search k search =
  let rec loop () =
    if search.safe_results >= k
    then
      (* We have enough results to return *)
      let results = ref [] in
      for i = search.min_dist - 1 downto 0 do
        results := search.results.(i) @ !results
      done;
      Util.take k !results
    else
      (* We need to carry on searching *)
      match pop_search_node search with
        (* Nothing left to search, return results *)
        | None ->
            let results = ref [] in
            for i = search.cutoff - 1 downto 0 do
              results := search.results.(i) @ !results
            done;
            Util.take k !results
        (* Search in bktree *)
        | Some bktree ->
            let (min_dist,max_dist) = query_node search bktree.root bktree.root_deleted in
            IntMap.iter
              (fun i node ->
                (* d is a lower bound for the distance to node, based on the triangle inequality *)
                let d = min search.min_dist (max (max_dist-i) 0) in
                push_search_node search node d)
              bktree.children;
            loop () in
  loop ()