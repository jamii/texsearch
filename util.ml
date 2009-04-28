let sum_float ls = List.fold_left (+.) 0. ls

let sum_int ls = List.fold_left (+) 0 ls

let minimum (l::ls) = List.fold_left min l ls

let maximum (l::ls) = List.fold_left max l ls

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