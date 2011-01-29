module type S =
sig
  include Map.S

  val update : key -> ('a -> 'a) -> 'a -> 'a t -> 'a t
  val count : 'a t -> int
  val to_list : 'a t -> (key * 'a) list
  val find_with : key -> 'a -> 'a t -> 'a
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
end

module Make (Ord : Map.OrderedType) : (S with type key = Ord.t)
