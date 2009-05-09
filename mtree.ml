type id = string

type node =
  { id : id
  ; latex : Latex.t
  ; suffixes : Latex.element array }

let node_of id latex =
    { id = id
    ; latex = latex
    ; suffixes = Edit.suffixes latex }

let query_dist a b = Edit.left_edit_distance a.suffixes b.suffixes

type branch =
  { neither : mtree
  ; left : mtree
  ; right : mtree
  ; both : mtree }

and root =
  { node : node
  ; deleted : bool }

and mtree =
  | Empty
  | Element of node
  | Branch of root * root * int * branch

let empty = Empty

let empty_branch =
  { left = empty
  ; both = empty
  ; right = empty
  ; neither = empty }

let root_of node =
  { node = node
  ; deleted = false }

let rec add node mtree =
  let rec loop mtree =
    match mtree with
    | Empty -> Element node
    | Element e ->
        Branch (root_of node, root_of e, (query_dist node e), empty_branch)
    | Branch (l,r,radius,branch) ->
        let branch = match ((query_dist node l.node) < radius, (query_dist node r.node) < radius) with
          | (false,false) -> {branch with neither = loop branch.neither}
          | (true, false) -> {branch with left    = loop branch.left}
          | (false,true ) -> {branch with right   = loop branch.right}
          | (true, true ) -> {branch with both    = loop branch.both} in
        Branch (l,r,radius,branch) in
   loop mtree

let delete id mtree =
  let rec loop mtree =
    match mtree with
    | Empty -> Empty
    | Element e ->
        if e.id = id
        then Empty
        else Element e
    | Branch (l,r,radius,branch) ->
        let l = if l.node.id = id then {l with deleted = true} else l in
        let r = if r.node.id = id then {r with deleted = true} else r in
        let branch =
          { neither = loop branch.neither
          ; left = loop branch.left
          ; right = loop branch.right
          ; both = loop branch.both } in
        Branch (l,r,radius,branch) in
   loop mtree

type search =
  { target : node
  ; unsearched : (mtree, int) Pqueue.t
  ; sorting : (id, int) Pqueue.t
  ; sorted : (id, int) Pqueue.t
  ; min_dist : int
  ; cutoff : int}

let search latex mtree =
  let e = node_of "" latex in
  { target = e
  ; unsearched =
      (match mtree with
        | Empty -> Pqueue.empty
        | _ -> Pqueue.add mtree 0 Pqueue.empty)
  ; sorting = Pqueue.empty
  ; sorted = Pqueue.empty
  ; min_dist = 0
  ; cutoff = Array.length (e.suffixes)}

let insert_result e dist search =
  if dist < search.cutoff
  then
    if dist < search.min_dist
    then {search with sorted = Pqueue.add e dist search.sorted}
    else {search with sorting = Pqueue.add e dist search.sorting}
  else search

let update_min_dist dist search =
  let min_dist = max search.min_dist dist in
  let (safe_results,rest) = Pqueue.split_at_priority min_dist search.sorting in
  {search with sorted = search.sorted @ safe_results; sorting = rest; min_dist = min_dist}

let next_search_node search =
  if search.min_dist > search.cutoff
  then None
  else
    match Pqueue.pop search.unsearched with
      | None -> None
      | Some ((mtree,dist),unsearched) ->
          Some (mtree, update_min_dist dist {search with unsearched = unsearched})

type result =
  | More of (id * int) list * search
  | Last of (id * int) list

let next k search =
  let rec loop search =
    match Pqueue.split_at_length k (search.sorted) with
      (* We have enough results to return *)
      | Some (results,rest) -> More (results, {search with sorted = rest})
      (* We need to carry on searching *)
      | None ->
          match next_search_node search with
            (* Nothing left to search *)
            | None ->
                (match search.sorting with
                  | [] -> Last (Pqueue.to_list search.sorted)
                  | _ -> loop {search with sorted = Pqueue.append search.sorted search.sorting; sorting = Pqueue.empty})
            (* Search in mtree *)
            | Some (mtree,search) ->
                match mtree with
                  | Empty -> loop search
                  | Element e -> loop (insert_result e.id (query_dist search.target e) search)
                  | Branch (l,r,radius,branch) ->
                      let distL = query_dist search.target l.node in
                      let distR = query_dist search.target r.node in
                      let search = if l.deleted then search else insert_result l.node.id distL search in
                      let search = if r.deleted then search else insert_result r.node.id distR search in
                      loop
                      {search with unsearched =
                        (Pqueue.add branch.neither 0
                        (Pqueue.add branch.left (distL - radius)
                        (Pqueue.add branch.right (distR - radius)
                        (Pqueue.add branch.both ((max distL distR) - radius)
                        search.unsearched))))} in
  loop search

let print mtree =
  let rec loop space mtree =
    print_string space;
    match mtree with
      | Empty -> print_string "( )\n"
      | Element _ -> print_string "(*)\n"
      | Branch (_,_,radius,branch) ->
          print_int radius; print_string "|\\\n";
          let space = "  "^space in
          loop space branch.neither;
          loop space branch.left;
          loop space branch.right;
          loop space branch.both in
  loop "" mtree