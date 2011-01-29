module type S =
sig
  include Map.S

  val update : key -> ('a -> 'a) -> 'a -> 'a t -> 'a t
  val count : 'a t -> int
  val to_list : 'a t -> (key * 'a) list
  val find_with : key -> 'a -> 'a t -> 'a
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
end

module Make (Ord : Map.OrderedType) : (S with type key = Ord.t) =
struct
  include Map.Make (Ord)

  let update key f default map =
    add key (try f (find key map) with Not_found -> default) map

  let count map = fold (fun _ _ n -> n+1) map 0

  let to_list map = fold (fun k v rest -> (k,v) :: rest) map []

  let find_with key default map = 
    try 
      find key map
    with Not_found ->
      default

  let filter_map f map =
    fold 
      (fun key value map -> 
        match (f value) with 
        | None -> map
        | Some value -> add key value map)
      map
      empty
end
