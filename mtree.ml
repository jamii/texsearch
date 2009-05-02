type index_element =
  { id : string
  ; fragment : Latex.fragment
  ; path_lengths : Histogram.t
  ; cost : float }

let index_element id fragment =
    { id = id
    ; fragment = fragment
    ; path_lengths = Histogram.path_lengths 10 fragment
    ; cost = Edit.cost_of_fragment fragment }

let weighted_metric metric a b = 1.0 /. (1.0 +. a.cost +. b.cost -. (metric a b))
let query_metric = (fun a b -> (Edit.with_cache Edit.edit_distance a.fragment b.fragment))
let index_metric = (fun a b -> float_of_int (Histogram.l1_norm a.path_lengths b.path_lengths))

let blob_cutoff = 0.5

type 'e branch =
  { neither : 'e mtree
  ; left : 'e mtree
  ; right : 'e mtree
  ; both : 'e mtree }

and 'e mtree =
  | Empty
  | Blob of 'e * ('e list)
  | Branch of 'e * 'e * float * 'e branch

let empty = Empty

let empty_branch =
  { left = empty
  ; both = empty
  ; right = empty
  ; neither = empty }

let rec add e mtree =
  let rec addE mtree =
    match mtree with
    | Empty -> Blob (e,[])
    | Blob (e',es) ->
        let dist = index_metric e e' in
        if dist < blob_cutoff
        then Blob (e',e::es)
        else List.fold_left (fun mtree e -> add e mtree) (Branch (e, e', (index_metric e e'), empty_branch)) es
    | Branch (l,r,radius,branch) ->
          let branch = match ((index_metric e l) < radius, (index_metric e r) < radius) with
            | (false,false) -> {branch with neither = addE branch.neither}
            | (true, false) -> {branch with left    = addE branch.left}
            | (false,true ) -> {branch with right   = addE branch.right}
            | (true, true ) -> {branch with both    = addE branch.both} in
          Branch (l,r,radius,branch) in
   addE mtree

type 'e search =
  { target : 'e
  ; unsearched : ('e mtree, float) Pqueue.t
  ; sorting : ('e, float) Pqueue.t
  ; sorted : ('e, float) Pqueue.t
  ; min_dist : float}

let search fragment mtree =
  let e = index_element "" fragment in
  { target = e
  ; unsearched =
      (match mtree with
        | Empty -> Pqueue.empty
        | _ -> Pqueue.add mtree 0.0 Pqueue.empty)
  ; sorting = Pqueue.empty
  ; sorted = Pqueue.empty
  ; min_dist = 0.0 }

let insert_result e dist cutoff search =
  if dist < cutoff
  then
    if dist < search.min_dist
    then {search with sorted = Pqueue.add e dist search.sorted}
    else {search with sorting = Pqueue.add e dist search.sorting}
  else search

let insert_results es cutoff search =
  List.fold_left (fun search e -> insert_result e (query_metric search.target e) cutoff search) search es

let update_min_dist dist search =
  let min_dist = max search.min_dist dist in
  let (safe_results,rest) = Pqueue.split_at_priority min_dist search.sorting in
  {search with sorted = search.sorted @ safe_results; sorting = rest; min_dist = min_dist}

let next_search_node cutoff search =
  if search.min_dist > cutoff
  then None
  else
    match Pqueue.pop search.unsearched with
      | None -> None
      | Some ((mtree,dist),unsearched) ->
          Some (mtree, update_min_dist dist {search with unsearched = unsearched})

type 'e result =
  | More of ('e * float) list * 'e search
  | Last of ('e * float) list

let next k cutoff search =
  let rec loop search =
    print_string "."; flush stdout;
    match Pqueue.split_at_length k (search.sorted) with
      (* We have enough results to return *)
      | Some (results,rest) -> More (results, {search with sorted = rest})
      (* We need to carry on searching *)
      | None ->
          match next_search_node cutoff search with
            (* Nothing left to search *)
            | None -> Last (Pqueue.to_list search.sorted)
            (* Search in mtree *)
            | Some (mtree,search) ->
                match mtree with
                  | Empty -> loop search
                  | Blob (e',es) -> loop (insert_results (e'::es) cutoff search)
                  | Branch (l,r,radius,branch) ->
                      let distL = query_metric search.target l in
                      let distR = query_metric search.target r in
                      loop
                      (insert_result l distL cutoff
                      (insert_result r distR cutoff
                      {search with unsearched =
                        (Pqueue.add branch.neither (max 0.0 (radius -. (min distL distR)))
                        (Pqueue.add branch.left (max 0.0 (distL -. radius))
                        (Pqueue.add branch.right (max 0.0 (distR -. radius))
                        (Pqueue.add branch.both (max 0.0 ((max distL distR) -. radius))
                        search.unsearched))))})) in
  loop search

let print_mtree mtree =
  let rec loop space mtree =
    print_string space;
    match mtree with
      | Empty -> print_string "( )\n"
      | Blob (_,es) -> print_string "("; print_int (List.length es); print_string ")\n"
      | Branch (_,_,radius,branch) ->
          print_float radius; print_string "|\\\n";
          let space = "  "^space in
          loop space branch.neither;
          loop space branch.left;
          loop space branch.right;
          loop space branch.both in
  loop "" mtree