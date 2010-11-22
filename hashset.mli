type 'a t

val create : int -> 'a t

val mem : 'a t -> 'a -> bool
val add : 'a t -> 'a -> unit

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t

val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t
