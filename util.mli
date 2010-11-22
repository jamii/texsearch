val flush_line : string -> unit

val minimum : 'a list -> 'a
val maximum : 'a list -> 'a

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val concat_map : ('a -> 'b list) -> 'a list -> 'b list

val range : int -> int -> int list

val list_of_stream : 'a Stream.t -> 'a list

val load_data : string -> 'a
val save_data : string -> 'a -> unit

val backtrace : (unit -> 'a) -> unit
