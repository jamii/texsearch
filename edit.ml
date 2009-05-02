open Util
open Latex

let metric a b =
  match (a,b) with
    | (None, Some b) -> 1.0
    | (Some a, None) -> 1.0
    | (Some a, Some b) ->
      match (a,b) with
        | (Text t1, Text t2) -> if t1 = t2 then 0.0 else 2.0
        | (Command (c1,_), Command (c2,_)) -> if c1 = c2 then 0.0 else 2.0
        | _ -> 2.0

let rec cost_of_fragment fragment =
  sum_float (List.map cost_of_element fragment)

and cost_of_element element =
  match element with
    | Text _ -> 1.0
    | Command (_,fragment) -> 1.0 +. cost_of_fragment fragment

let rec edit_distance cached fL fR =
  match (fL,fR) with
    | ([], []) -> 0.0
    | (csL, []) -> cost_of_fragment csL
    | ([], csR) -> cost_of_fragment csR
    | (cL::csL, cR::csR) ->
        minimum
          [ (metric None (Some cR)) +. (cached fL ((children cR) @ csR))
          ; (metric (Some cL) None) +. (cached ((children cL) @ csL) fR)
          ; (metric (Some cL) (Some cR)) +. (cached ((children cL) @ csL) ((children cR) @ csR)) ]

module CacheMap = Map.Make (
struct
  type t = (Latex.fragment * Latex.fragment)
  let compare = compare
end)

let with_cache f =
  let cache = ref CacheMap.empty in
  let rec cached a b =
    try
      CacheMap.find (a,b) !cache
    with Not_found ->
      let result = f cached a b in
      cache := CacheMap.add (a,b) result !cache;
      result in
  cached