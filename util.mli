val flush_line : string -> unit

val minimum : 'a list -> 'a
val maximum : 'a list -> 'a

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val concat_map : ('a -> 'b list) -> 'a list -> 'b list

val range : int -> int -> int list

val list_of_stream : 'a Stream.t -> 'a list

val load_data : string -> 'a
val save_data : string -> 'a -> unit

module type MAP =
sig
  include Map.S

  val update : key -> ('a -> 'a) -> 'a -> 'a t -> 'a t
  val count : 'a t -> int
  val to_list : 'a t -> (key * 'a) list
  val find_with : key -> 'a -> 'a t -> 'a
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
end

module Make_map (Ord : Map.OrderedType) : (MAP with type key = Ord.t)

val backtrace : (unit -> 'a) -> unit
