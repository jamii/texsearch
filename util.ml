let minimum (l::ls) = List.fold_left min l ls

let maximum (l::ls) = List.fold_left max l ls

let filter_map f ls =
  List.map 
    (fun l -> match l with Some a -> a) 
    ((List.filter 
      (fun l -> l != None) 
      (List.map f ls)))

let rec take k ls =
  if k <= 0 then [] else
  match ls with
    | [] -> []
    | (l::ls) -> l :: take (k-1) ls

let rec drop k ls =
  if k <= 0 then ls else
  match ls with
    | [] -> []
    | (l::ls) -> drop (k-1) ls
