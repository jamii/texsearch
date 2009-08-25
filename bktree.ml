(*
The data structure used for the index is a modified bktree.
Each tree node contains a single document and its associated latex strings.
The children of the node are stored according to their distance (index_dist) from the node.

The left_edit_distance satisfies the triangle inequality: for any a,b,c
  left_edit_dist a c <= left_edit_dist a b + left_edit_dist b c
From this we can derive:
  min_j (query_dist query b_j) >= min_i (query_dist query c_i) - min_i max_j (left_edit_dist b_j c_i)
Where i and j range over all the equations in the documents b and c
This is the crucial fact that allows efficient searching within the tree.
*)

type doi = string
type id = string

module DoiMap = Map.Make (struct
  type t = doi
  let compare = compare
end)

module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

(* A single equation *)
type node =
  { doi : doi
  ; equation : id * Latex.t }

let node_of doi equation =
  { doi = doi
  ; equation = equation }

(* Extending the edit distance on latex strings to edit distance on compound queries *)

open Query

let rec query_dist query latex =
  match query with
    | Latex (query_latex,_) -> Edit.left_edit_distance query_latex latex
    | And (query1,query2) -> max (query_dist query1 latex) (query_dist query2 latex)
    | Or (query1,query2) -> min (query_dist query1 latex) (query_dist query2 latex)
    | Not query -> query_dist_not query latex

and query_dist_not query latex =
  match query with
    | Latex (query_latex,_) -> (Array.length query_latex) - (Edit.left_edit_distance query_latex latex)
    | And (query1,query2) -> min (query_dist_not query1 latex) (query_dist_not query2 latex)
    | Or (query1,query2) -> max (query_dist_not query1 latex) (query_dist_not query2 latex)
    | Not query -> query_dist query latex

(* Return each equation whose distance to the query is less than the cutoff, as well as the min distance *)
let query_against query equations cutoff =
  let (min_dist, ranked_equations) = 
    List.fold_left
      (fun (min_dist,matches) (id,latex) ->
        let dist = query_dist query latex in
        if dist < cutoff then (min dist min_dist, (id,dist) :: matches) else (min dist min_dist, matches))
      (max_int,[])
      equations in
  (min_dist, List.sort (fun a b -> compare (snd a) (snd b)) ranked_equations)

(* A bk cover tree *)
type bktree =
  { root : node
  ; children : bktree IntMap.t }

let rec size bktree = 1 + (IntMap.fold (fun _ child total -> total + size child) bktree.children 0)

(* A dummy node sits at the top of the tree. *)
let empty =
  { root = node_of "" ("",Latex.empty ())
  ; children = IntMap.empty }

let empty_branch node =
  { root = node
  ; children = IntMap.empty }

let to_bucket d = d / 3
let from_bucket d = (d + 1) * 3

let rec add node bktree =
  let d = to_bucket (Edit.left_edit_distance (snd node.equation) (snd bktree.root.equation)) in
  let children =
    try
      IntMap.add d (add node (IntMap.find d bktree.children)) bktree.children
    with Not_found ->
      IntMap.add d (empty_branch node) bktree.children in
  {bktree with children=children}

let rec descendants bktree =
  IntMap.fold (fun _ child nodes -> child.root :: (List.append (descendants child) nodes)) bktree.children []

let rec delete doi bktree =
  (* Remove each child which matches the doi and make a list of the removed children *)
  let (children,deletees) = 
    IntMap.fold
      (fun dist child (children,deletees) ->
        if child.root.doi = doi
        then (children, child::deletees)
        else (IntMap.add dist (delete doi child) children, deletees))
      bktree.children
      (IntMap.empty,[]) in
  (* Some descendants of the the removed children might need be added back in *)
  List.fold_left
    (fun bktree child -> 
      let addable_descendants = List.filter (fun node -> node.doi != doi) (descendants child) in
      List.fold_left 
        (fun bktree descendant -> add descendant bktree)
        bktree
        addable_descendants)
    {root=bktree.root; children=children}
    deletees

(* Simple introspection *)

let doi_map bktree =
  List.fold_left
    (fun doi_map node ->
      try
        DoiMap.add node.doi (node.equation :: DoiMap.find node.doi doi_map) doi_map
      with Not_found ->
        DoiMap.add node.doi [node.equation] doi_map) 
    DoiMap.empty
    (descendants bktree)

(*
The search structure tracks the progress of a search through a tree.
The search will eventually return every document node whose distance to the query is less than the cutoff.

search.unsearched stores tree nodes which have yet to be searched.
Tree nodes in search.unsearched.(i) are guaranteed not to contain any document nodes with distance < i from the query.

search.min_dist tracks the smallest non-empty index in search.unsearches,
so every document node remaining in the search has distance >= min_dist.

search.results stores matches for the query.
search.results.(i) contains matches with dist i from the query.

search.safe_results stores the doi for each article which has an equation with distance < min_dist.
no as-yet-unsearched results will match more closely than these
so if the number of results required is less than the size search.safe_results of it is safe to stop searching
*)

type result = doi * (id * int)

type search =
  { query : Query.t
  ; cutoff : int
  ; unsearched : bktree list array
  ; results : result list array
  ; mutable safe_results : ((id*int) list) DoiMap.t
  ; mutable min_dist : int }

exception Broken

(* Query a document node, update the search structure, return the min distance from the query *)
let query_node search node =
  let dist = query_dist search.query (snd node.equation) in
  if dist < search.cutoff
  then search.results.(dist) <- (node.doi,(fst node.equation, dist)) :: search.results.(dist)
  else ();
  dist

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
      search.safe_results <- 
        List.fold_left 
          (fun safe_results (doi,eqn) -> 
            try
              DoiMap.add doi (eqn :: DoiMap.find doi safe_results) safe_results
            with Not_found ->
              DoiMap.add doi [eqn] safe_results) 
          search.safe_results
          search.results.(search.min_dist);
      search.min_dist <- search.min_dist + 1;
      pop_search_node search
    | (bktree::rest) ->
      (* Search in bktree, put the rest back *)
      search.unsearched.(search.min_dist) <- rest;
      Some bktree

(* The choice of cutoff is completely arbitrary *)
let cutoff_length query = 1 + (min 5 (Query.max_length query / 3))

(* The initial search structure *)
let new_search query bktree =
  let cutoff = cutoff_length query  in
  let search =
    { query = query
    ; cutoff = cutoff
    ; unsearched = Array.make cutoff []
    ; results = Array.make cutoff []
    ; safe_results = DoiMap.empty
    ; min_dist = 0 } in
  push_search_node search bktree 0;
  search

(* Runs search until at least k results can be returned *)
let run_search k search =
  let rec loop () =
    if (DoiMap.fold (fun _ _ total -> 1 + total) search.safe_results 0) >= k
    then
      (* We have enough results to return *)
      DoiMap.fold (fun doi eqns acc -> (doi,eqns)::acc) search.safe_results []
    else
      (* We need to carry on searching *)
      match pop_search_node search with
        (* Nothing left to search, return results *)
        | None ->
            for i = search.cutoff - 1 downto search.min_dist do
              search.safe_results <- 
                List.fold_left 
                  (fun safe_results (doi,eqn) -> 
                    try
                      DoiMap.add doi (eqn :: DoiMap.find doi safe_results) safe_results
                    with Not_found ->
                      DoiMap.add doi [eqn] safe_results) 
                  search.safe_results
                  search.results.(i)
            done;
            Util.take k (DoiMap.fold (fun doi eqns acc -> (doi,eqns)::acc) search.safe_results [])
        (* Search in bktree *)
        | Some bktree ->
            let dist = query_node search bktree.root in
            IntMap.iter
              (fun i node ->
                (* d is a lower bound for the distance to node, based on the triangle inequality *)
                let d = max search.min_dist (dist-(from_bucket i)) in
                push_search_node search node d)
              bktree.children;
            loop () in
  loop ()
