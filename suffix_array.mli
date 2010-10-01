type 'a t

val empty : unit -> 'a t
val add : 'a t -> ('a * Latex.t) list -> unit
val find_exact : 'a t -> Latex.t -> 'a list
val find_approx : 'a t -> Latex.t -> int -> (int * 'a) list
