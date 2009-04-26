let sum ls = List.fold_left (+) 0 ls

let minimum ls = List.fold_left min max_int ls

let maximum ls = List.fold_left max min_int ls

let partitions rs =
  let rec loop ls rs =
    (Dlist.to_list ls, rs) ::
    match rs with
      | [] -> []
      | r::rs -> loop (Dlist.snoc r ls) rs in
  loop Dlist.empty rs

let filter_map f ls =
  List.map (fun l -> match l with Some a -> a) (
  (List.filter (fun l -> l != None) (
  List.map f ls)))