let flush_line str = print_string str; print_string "\n"; flush stdout

let minimum (l::ls) = List.fold_left min l ls

let maximum (l::ls) = List.fold_left max l ls

let filter_map f ls =
  List.map 
    (fun l -> match l with Some a -> a) 
    ((List.filter 
      (fun l -> l <> None) 
      (List.map f ls)))

let concat_map f ls =
  List.fold_right (@) (List.map f ls) []

let rec range start finish = 
  if start < finish then start :: range (start+1) finish else []

(* Fairly hackish method of sucking out stream elements *)
let list_of_stream stream = Stream.npeek max_int stream 

module type MAP =
sig
  include Map.S

  val update : key -> ('a -> 'a) -> 'a -> 'a t -> 'a t
  val count : 'a t -> int
  val to_list : 'a t -> (key * 'a) list
  val find_with : key -> 'a -> 'a t -> 'a
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
end

module Make_map (Ord : Map.OrderedType) : (MAP with type key = Ord.t) =
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

(* Tune the gc for lots of garbage *)
open Gc
let expect_garbage () =
  let m = 1024 * 1024 in
  Gc.set 
    {Gc.get () with
      minor_heap_size = 256 * m;
      major_heap_increment = 64 * m;
      space_overhead = 200
    }

