open Latex

type doi = string
type id = string

type node =
  { doi : doi
  ; equations : (id * Latex.t) list }

let node_of doi equations =
    { doi = doi
    ; equations = equations }

let query_dist a bl =
  Util.minimum (List.map (fun (_,b) ->
    Edit.left_edit_distance a b)
  bl)

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
  ; mutable bucket : node list
  ; mutable children : bktree IntMap.t }

let empty =
  { root = node_of "" [("",Array.create 0 0)]
  ; root_deleted = true
  ; bucket = []
  ; children = IntMap.empty }

let empty_branch node =
  { root = node
  ; root_deleted = false
  ; bucket = []
  ; children = IntMap.empty }

let rec add node bktree =
  let d = index_dist node.equations bktree.root.equations in
  if d = 0
  then bktree.bucket <- node :: bktree.bucket
  else
    try
      add node (IntMap.find d bktree.children)
    with Not_found ->
      bktree.children <- IntMap.add d (empty_branch node) bktree.children

let rec delete doi bktree =
  if bktree.root.doi = doi then bktree.root_deleted <- true else ();
  bktree.bucket <- List.filter (fun node -> node.doi != doi) bktree.bucket;
  IntMap.iter (fun _ child -> delete doi child) bktree.children

type search =
  { target : Latex.t
  ; cutoff : int
  ; unsearched : bktree list array (* index nodes left to search *)
  ; results : doi list array (* results.(i) is results with cost distance i *)
  ; mutable safe_results : int (* number of results with distance < min_dist *)
  ; mutable min_dist : int (* minimum distance to an unsearched node *)}

let insert_result d id search =
  if d >= search.cutoff then () else
  (search.results.(d) <- id :: search.results.(d);
  if d < search.min_dist then search.safe_results <- search.safe_results + 1 else ())

let insert_results nodes search =
  List.iter (fun node -> insert_result (query_dist search.target node.equations) node.doi search) nodes

let insert_unsearched_node d node search =
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
  IntMap.iter (fun i node -> insert_unsearched_node ((Array.length latex)-i) node search) bktree.children;
  search

let rec next_search_node search =
  if search.min_dist >= search.cutoff then None else
  match search.unsearched.(search.min_dist) with
    | [] ->
      search.safe_results <- search.safe_results + List.length(search.results.(search.min_dist));
      search.min_dist <- search.min_dist + 1;
      next_search_node search
    | (bktree::rest) ->
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
        (* Nothing left to search *)
        | None ->
            let results = ref [] in
            for i = search.cutoff - 1 downto 0 do
              results := search.results.(i) @ !results
            done;
            Util.take k !results
        (* Search in bktree *)
        | Some bktree ->
            (let d = query_dist search.target bktree.root.equations in
            IntMap.iter (fun i node -> insert_unsearched_node (d-i) node search) bktree.children;
            if bktree.root_deleted then () else insert_result d bktree.root.doi search;
            insert_results bktree.bucket search;
            loop ()) in
  loop ()