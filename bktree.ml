open Latex

type id = string

type node =
  { id : id
  ; latex : Latex.t }

let node_of id latex =
    { id = id
    ; latex = latex }

let dist a b = Edit.left_edit_distance a.latex b.latex

type bktree =
  | Empty
  | Branch of node * bool * node list * bktree array

let branch_size = 20
let bucket_size = 5

let empty_branch node =
  Branch (node, false, [], Array.make (branch_size + 1) Empty)

let add node bktree =
  let rec add_node bktree =
    match bktree with
    | Empty -> empty_branch node
    | Branch (root,del,bucket,branch) ->
        let d = dist node root in
        if d < bucket_size
        then Branch (root,del,node::bucket,branch)
        else
          let i = min (d / bucket_size) branch_size in
          branch.(i) <- add_node branch.(i);
          bktree in
  add_node bktree

let delete id bktree =
  let rec loop bktree =
    match bktree with
    | Empty -> Empty
    | Branch (root,del,bucket,branch) ->
        let del = if root.id = id then true else del in
        let bucket = List.filter (fun node -> node.id != id) bucket in
        for i = 0 to branch_size do
          branch.(i) <- loop branch.(i)
        done;
        Branch (root,del,bucket,branch) in
  loop bktree

type search =
  { target : node
  ; unsearched : (bktree, int) Pqueue.t
  ; sorting : (id, int) Pqueue.t
  ; sorted : (id, int) Pqueue.t
  ; min_dist : int
  ; cutoff : int }

let search latex bktree =
  let node = node_of "" latex in
  { target = node
  ; unsearched =
      (match bktree with
        | Empty -> Pqueue.empty
        | Branch _ -> Pqueue.add bktree 0 Pqueue.empty)
  ; sorting = Pqueue.empty
  ; sorted = Pqueue.empty
  ; min_dist = 0
  ; cutoff = (Array.length (node.latex) / 3) + 1}

let insert_result node d search =
  if d < search.cutoff
  then
    if d < search.min_dist
    then {search with sorted = Pqueue.add node d search.sorted}
    else {search with sorting = Pqueue.add node d search.sorting}
  else search

let insert_results nodes search =
  List.fold_left (fun search node -> insert_result node.id (dist search.target node) search) search nodes

let update_min_dist d search =
  let min_dist = max search.min_dist d in
  let (safe_results,rest) = Pqueue.split_at_priority min_dist search.sorting in
  {search with sorted = search.sorted @ safe_results; sorting = rest; min_dist = min_dist}

let next_search_node search =
  if search.min_dist > search.cutoff
  then None
  else
    match Pqueue.pop search.unsearched with
      | None -> None
      | Some ((bktree,d),unsearched) ->
          Some (bktree, update_min_dist d {search with unsearched = unsearched})

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
            (* Search in bktree *)
            | Some (bktree,search) ->
                match bktree with
                  (* Empty trees never make it into the search queue *)
                  | Branch (root,del,bucket,branch) ->
                      let d = dist search.target root in
                      let unsearched = ref search.unsearched in
                      (for i = 0 to branch_size - 1 do
                        match branch.(i) with
                          | Empty -> ()
                          | Branch _ as b -> unsearched := Pqueue.add b (d - (i*bucket_size)) !unsearched
                      done);
                      (match branch.(branch_size) with
                          | Empty -> ()
                          | Branch _ as b -> unsearched := Pqueue.add b 0 !unsearched);
                      let search = if del then search else insert_result root.id d search in
                      loop (insert_results bucket {search with unsearched = !unsearched}) in
  loop search