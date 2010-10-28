type t = int array

val empty : unit -> t
val length : t -> int

val of_array : int array -> t

val of_json : Json_type.t -> t
val lines_of_json : Json_type.t -> t list
val to_json : t -> Json_type.t

type pos = int

val compare_suffix : (t * pos) -> (t * pos) -> int
val is_prefix : (t * pos) -> (t * pos) -> bool
val fragments : t -> int -> t list
