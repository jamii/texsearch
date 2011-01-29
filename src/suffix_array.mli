type id = int
type pos = int

type 'a t = 
  { latexs : Latex.t DynArray.t 
  ; opaques : 'a DynArray.t
  ; deleted : bool DynArray.t
  ; mutable next_id : id
  ; mutable array : Suffix.t array 
  ; mutable unsorted :  ('a * Latex.t) list }

val create : unit -> 'a t
val ancientify : 'a t -> unit

val add : 'a t -> ('a * Latex.t) list -> unit
val prepare : 'a t -> unit

val delete : 'a t -> ('a -> bool) -> unit

val find_exact : 'a t -> Latex.t -> (int * 'a) list
val find_approx : 'a t -> float -> Latex.t -> (int * 'a) list
val find_query : 'a t -> float -> Query.t -> (int * 'a) list
