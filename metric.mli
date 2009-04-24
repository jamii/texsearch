module type S =
sig
  type t
  val metric : t -> t -> int
end