open Latex

type doi = string
type id = string

type node =
  { doi : doi
  ; equations : (id * Latex.t) list }

let node_of doi equations =
    { doi = doi
    ; equations = equations }

let query_against query equations cutoff =
  List.fold_left
    (fun (min_dist,matches) (id,latex) ->
      let dist = Edit.left_edit_distance query latex in
      if dist < cutoff then (min dist min_dist, id :: matches) else (min_dist, matches))
    (cutoff,[])
    equations

let index_dist al bl =
  Util.maximum (List.map (fun (_,a) ->
    Util.maximum (List.map (fun (_,b) ->
      Edit.left_edit_distance a b)
    bl))
  al)

module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

type bktree =
  { root : node
  ; mutable root_deleted : bool
  ; mutable children : bktree IntMap.t }

let empty =
  { root = node_of "" [("",Array.create 0 0)]
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

type search =
  { target : Latex.t
  ; cutoff : int
  ; unsearched : bktree list array (* index nodes left to search *)
  ; results : result list array (* results.(i) is results with cost distance i *)
  ; mutable safe_results : int (* number of results with distance < min_dist *)
  ; mutable min_dist : int (* minimum distance to an unsearched node *)}

let query_node search node node_deleted =
  let (dist,matches) = query_against search.target node.equations search.cutoff in
  if dist < search.cutoff && not node_deleted
  then search.results.(dist) <- (node.doi,matches) :: search.results.(dist)
  else ();
  if dist < search.min_dist
  then search.safe_results <- search.safe_results + 1
  else ();
  dist

let queue_node search node d =
  if d >= search.cutoff then () else
  let d = min search.min_dist (max d 0) in
  search.unsearched.(d) <- node :: search.unsearched.(d)

let new_search latex bktree =
  let cutoff = 1 + (Array.length latex / 3)  in
  let search =
    { target = latex
    ; cutoff = cutoff
    ; unsearched = Array.make cutoff []
    ; results = Array.make cutoff []
    ; safe_results = 0
    ; min_dist = 0 } in
  IntMap.iter (fun i node -> queue_node search node (Array.length latex - i)) bktree.children;
  search

let rec next_search_node search =
  if search.min_dist >= search.cutoff then None else
  match search.unsearched.(search.min_dist) with
    | [] ->
      (* Nothing left at this distance *)
      search.safe_results <- search.safe_results + List.length(search.results.(search.min_dist));
      search.min_dist <- search.min_dist + 1;
      next_search_node search
    | (bktree::rest) ->
      (* Search in bktree, put the rest back *)
      search.unsearched.(search.min_dist) <- rest;
      Some bktree

let next k search =
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
      match next_search_node search with
        (* Nothing left to search, return results *)
        | None ->
            let results = ref [] in
            for i = search.cutoff - 1 downto 0 do
              results := search.results.(i) @ !results
            done;
            Util.take k !results
        (* Search in bktree *)
        | Some bktree ->
            let dist = query_node search bktree.root bktree.root_deleted in
            IntMap.iter (fun i node -> queue_node search node (dist-i)) bktree.children;
            loop () in
  loop ()