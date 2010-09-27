(* 
A quasi-metric space is a set of points and a distance function satisfying:
  For all a. dist a a = 0
  For all a,b. dist a b >= 0
  For all a,b,c. dist a c <= dist a b + dist b c

The data structure used for the index is a bk-cover-tree.
The tree organises a quasi-metric space by successive subdivisions of the space.
Each tree node contains a single point and its children are arranged by distance to that point.
This structure allows for efficient searching by any 'query : node -> int' which satisfies:
  For all a. query a >= 0
  For all a, b. query b - query a <= dist a b

A simple example of such a query is the function:
  let query a = dist q a
for some node q.
*)

module type QUASI_METRIC =
sig
  type t
  val dist : t -> t -> int
  (* For all a. dist a a = 0
     For all a,b. dist a b >= 0
     For all a,b,c. dist a c <= dist a b + dist b c *)
end

module type BKTREE =
functor (Quasi_metric : QUASI_METRIC) ->
sig
  type node = Quasi_metric.t
  type t
  val empty_branch : node -> t
  val size : t -> int
  val add : node -> t -> t
  val descendants : t -> node list
  val to_list : t -> node list
  val of_list : node list -> t
  val filter : (node -> bool) -> t -> t option
  
  type result = node * int
  val run_query : (node -> int) -> int -> t -> result Stream.t
end

module Make : BKTREE =
functor (Quasi_metric : QUASI_METRIC) ->
struct 

  type node = Quasi_metric.t

  let dist = Quasi_metric.dist

  module IntMap = 
  struct
    include Map.Make (struct
      type t = int
      let compare = compare
    end)

    let update key f default map =
      add key (try f (find key map) with Not_found -> default) map

    let filter_map f map =
      fold 
        (fun key value map -> 
          match (f value) with 
            | None -> map
            | Some value -> add key value map)
        map
        empty
  end

  (* A bk-cover-tree *)
  (* This cannot represent an empty bktree, a fact which is convenient except when deleting nodes *) 
  type t =
    { root_node : node
    ; pivot : int option
    ; sizes : int IntMap.t
    ; children : t IntMap.t }

  let empty_branch node = 
    { root_node = node
    ; pivot = None
    ; sizes = IntMap.empty
    ; children = IntMap.empty }

  let rec size bktree = 1 + (IntMap.fold (fun _ child total -> total + size child) bktree.children 0)

  (* from_bucket d = max { x | to_bucket x = d } *)
  let to_bucket pivot d = if d <= pivot then 0 else 1
  let from_bucket pivot d = if d == 0 then pivot else max_int

  let rec add_raw node bktree =
    let d = dist node bktree.root_node in
    let pivot = 
      match bktree.pivot with
      |	None -> d
      |	Some pivot -> pivot in
    let bucket = to_bucket pivot d in
    let sizes = IntMap.update bucket ((+) 1) 0 bktree.sizes in
    let children = IntMap.update bucket (add node) (empty_branch node) bktree.children in
    {bktree with pivot=Some pivot; sizes=sizes; children=children}

  and add node bktree =
    balance (add_raw node bktree)

  and of_list (root_node::nodes) =
    let dists = List.map (fun node -> dist node root_node) nodes in
    let n = List.length nodes in
    let pivot = List.nth dists ((n-1)/2) in
    let bktree = {empty_branch root_node with pivot=Some pivot} in
    List.fold_left (fun bktree node -> add_raw node bktree) bktree nodes

  and to_list bktree =
    bktree.root_node :: descendants bktree

  and descendants bktree = IntMap.fold (fun _ -> List.append) (IntMap.map to_list bktree.children) []

  and balance bktree =
    let left = IntMap.find 0 bktree.sizes in
    let right = IntMap.find 1 bktree.sizes in
    if left < right
    then 
      begin
	Util.flush_line ("Balancing " ^ (string_of_int left) ^ " " ^ (string_of_int right));
	of_list (to_list bktree)
      end
    else bktree

  (* This is an expensive function. Since there is no way to merge bktree's, everything below a deleted node must be rebuilt *)
  (* Since we have no way to represent an empty tree, this may have to return None *)
  let rec filter f bktree =
    if f bktree.root_node
    then 
      (* Just filter children *)
      let children = IntMap.filter_map (filter f) bktree.children in
      Some {bktree with sizes=IntMap.map size children; children=children}
    else
      (* Have to rebuild everything *)
      let nodes = List.filter f (descendants bktree) in
      match nodes with
        | [] -> None
        | _  -> Some (of_list nodes)

  (* 
  the search structure tracks the progress of a search through a tree
  the search will eventually return every node st 'query node < cutoff'

  search.unsearched stores branches which have yet to be searched
  for all node in all branches in search.unsearched.(i) we guarantee that 'query node >= i'

  search.min_dist tracks the smallest non-empty index in search.unsearched
  so every unsearched node remaining satisfies 'query node >= search.min_dist'

  search.results stores nodes which have been searched
  search.results.(i) contains matches with dist i from the query

  the search continues until search.min_dist = cutoff at which point we know every unsearched node satisfies 'query node >= cutoff' 
  *)

  type result = node * int 

  type search =
    { query : node -> int (* Requires for any a,b: query a >= query b - dist a b *)
    ; cutoff : int
    ; unsearched : t list array
    ; results : result list array
    ; mutable min_dist : int }

  (* Push a tree node onto the search queue *)
  let push_search_node search node d =
    if d < search.cutoff then search.unsearched.(d) <- node :: search.unsearched.(d)

  (* Processes search nodes until search.min_dist increases at which point at group of results can be safely returned *)
  let rec more_results search =
    match search.unsearched.(search.min_dist) with
      | [] ->
        (* Nothing left at this distance, safe to return some results *)
        let results = search.results.(search.min_dist) in
        search.min_dist <- search.min_dist + 1;
        results
      | (bktree::rest) ->
        (* Search in bktree, put the rest back *)
        search.unsearched.(search.min_dist) <- rest;
        let d = search.query bktree.root_node in
	if d < search.cutoff then search.results.(d) <- (bktree.root_node, d) :: search.results.(d);
	begin match bktree.pivot with 
	| None -> ()
	| Some pivot ->
            IntMap.iter
              (* a lower bound for the distance to node, based on the triangle inequality *)
              (fun bucket node -> push_search_node search node (max search.min_dist (d-(from_bucket pivot bucket))))
              bktree.children end;
	more_results search

  (* Returns all nodes s.t query node < cutoff *)
  (* Requires: For all a. query a >= 0
               For all a, b. query b - query a <= dist a b *)
  let run_query query cutoff bktree =
    let search =
      { query = query
      ; cutoff = cutoff
      ; unsearched = Array.make cutoff []
      ; results = Array.make cutoff []
      ; min_dist = 0 } in
    push_search_node search bktree 0;
    let result_buffer = ref [] in
    let rec next_result i =
      match !result_buffer with
        | [] -> 
            if search.min_dist >= cutoff then None 
            else (result_buffer := more_results search; next_result i)
        | (result :: results) -> result_buffer := results; Some result in
    Stream.from next_result

end
