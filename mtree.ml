let metric a b = Cache.with_cache Edit.edit_distance a b

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

let add e mtree =
  print_string "."; flush stdout;
  let rec addE mtree =
    match mtree with
      | Empty -> Element e
      | Element e' -> Branch (e, e', (metric e e'), empty_branch)
      | Branch (l,r,radius,branch) ->
          let branch = match ((metric e l) < radius, (metric e r) < radius) with
            | (false,false) -> {branch with neither = addE branch.neither}
            | (true, false) -> {branch with left    = addE branch.left}
            | (false,true ) -> {branch with right   = addE branch.right}
            | (true, true ) -> {branch with both    = addE branch.both} in
          Branch (l,r,radius,branch) in
   addE mtree

(* Use dlist appends or this will blow up*)
let rec find_within dist e mtree =
    print_string "."; flush stdout;
    match mtree with
    | Empty -> []
    | Element e' ->
        let distE = metric e e' in
        if distE < dist
        then [(distE,e')]
        else []
    | Branch (l,r,radius,branch) ->
        let distL, distR = metric e l, metric e r in
        (if distL < dist then [(distL,l)] else []) @
        (if distR < dist then [(distR,r)] else []) @
        (if (distL >= radius - dist) && (distR >= radius - dist) then find_within dist e branch.neither else []) @
        (if (distL <  radius + dist) && (distR >= radius - dist) then find_within dist e branch.left    else []) @
        (if (distL >= radius - dist) && (distR <  radius + dist) then find_within dist e branch.right   else []) @
        (if (distL <  radius - dist) && (distR <  radius - dist) then find_within dist e branch.both    else [])

let make_index str = List.fold_left (fun mtree e -> add e mtree) empty (Tree.parse_results str)
