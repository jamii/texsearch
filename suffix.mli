type id = int
type pos = int

type t

val max_size : int

exception Invalid_suffix of id * pos

val pack : id * pos -> t
val unpack : t -> id * pos
