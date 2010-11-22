type t =
  | Latex of Latex.t * string (* Store the string version so we can send the query back to the users *)
  | And of t * t
  | Or of t * t

exception Parse_error

val of_string : (string -> (Latex.t * string)) -> string -> t
val to_string : t -> string

val distance : t -> Latex.t -> int
val similar : float -> t -> Latex.t -> int option
