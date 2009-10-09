let minimum (l::ls) = List.fold_left min l ls

let maximum (l::ls) = List.fold_left max l ls

let filter_map f ls =
  List.map 
    (fun l -> match l with Some a -> a) 
    ((List.filter 
      (fun l -> l != None) 
      (List.map f ls)))

(* Fairly hackish method of sucking out stream elements *)
let list_of_stream stream = Stream.npeek max_int stream 

