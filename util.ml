let minimum (l::ls) = List.fold_left min l ls

let maximum (l::ls) = List.fold_left max l ls

let filter_map f ls =
  List.map 
    (fun l -> match l with Some a -> a) 
    ((List.filter 
      (fun l -> l <> None) 
      (List.map f ls)))

(* Fairly hackish method of sucking out stream elements *)
let list_of_stream stream = Stream.npeek max_int stream 

module Make_map = 
functor (Ord : Map.OrderedType) ->
struct
  include Map.Make (Ord)

  let update key f default map =
    add key (try f (find key map) with Not_found -> default) map

  let count map = fold (fun _ _ n -> n+1) map 0

  let list_of map = fold (fun k v rest -> (k,v) :: rest) map []
end

