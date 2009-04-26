open Tree

type index_element =
  { forest : element forest
  ; path_lengths : Histogram.t }

let query_metric a b = Cache.with_cache Edit.edit_distance a.forest b.forest
let index_metric a b = Histogram.l1_norm a.path_lengths b.path_lengths

type 'e branch =
  { neither : 'e mtree
  ; left : 'e mtree
  ; right : 'e mtree
  ; both : 'e mtree }

and 'e mtree =
  | Empty
  | Element of 'e
  | Branch of 'e * 'e * int * 'e branch


let empty = Empty

let empty_branch =
  { left = empty
  ; both = empty
  ; right = empty
  ; neither = empty }

let add forest mtree =
  let e =
    { forest = forest
    ; path_lengths = Histogram.path_lengths 10 forest } in
  print_string "."; flush stdout;
  let rec addE mtree =
    match mtree with
    | Empty -> Element e
    | Element e' -> Branch (e, e', (index_metric e e'), empty_branch)
    | Branch (l,r,radius,branch) ->
          let branch = match ((index_metric e l) < radius, (index_metric e r) < radius) with
            | (false,false) -> {branch with neither = addE branch.neither}
            | (true, false) -> {branch with left    = addE branch.left}
            | (false,true ) -> {branch with right   = addE branch.right}
            | (true, true ) -> {branch with both    = addE branch.both} in
          Branch (l,r,radius,branch) in
   addE mtree

(* Use dlist appends or this will blow up*)
let find_within dist forest mtree =
  let e =
    { forest = forest
    ; path_lengths = Histogram.path_lengths 10 forest } in
  let rec find_in mtree =
    print_string "."; flush stdout;
    match mtree with
    | Empty -> []
    | Element e' ->
        let distE = index_metric e e' in
        if distE < dist then [e'] else []
    | Branch (l,r,radius,branch) ->
        let distL, distR = index_metric e l, index_metric e r in
        (if distL < dist then [l] else []) @
        (if distR < dist then [r] else []) @
        (if (distL >= radius - dist) && (distR >= radius - dist) then find_in branch.neither else []) @
        (if (distL <  radius + dist) && (distR >= radius - dist) then find_in branch.left    else []) @
        (if (distL >= radius - dist) && (distR <  radius + dist) then find_in branch.right   else []) @
        (if (distL <  radius - dist) && (distR <  radius - dist) then find_in branch.both    else []) in
  Util.filter_map (fun e' -> let rank = query_metric e e' in if rank < dist then Some (rank,e'.forest) else None) (find_in mtree)

let make_index str = List.fold_left (fun mtree e -> add e mtree) empty (Tree.parse_results str)
