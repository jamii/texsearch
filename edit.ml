open Util
open Latex

let metric a b =
  match (a,b) with
    | (Text ta, Text tb) -> if ta = tb then 0 else 5
    | (Command (ca,_), Command (cb,_)) -> if ca = cb then 0 else 5
    | _ -> 5

let suffixes forest =
  let rec loop forest =
    match forest with
      | [] -> []
      | (t::ts) -> t :: (loop ((children t) @ ts)) in
  Array.of_list (loop forest)

(*let edit_distance forestL forestR =
  let suffixL, suffixR = suffixes forestL, suffixes forestR in
  let maxl, maxr = Array.length suffixL, Array.length suffixR in
  let cache = Array.make_matrix (maxl + 1) (maxr + 1) 0 in
  for l = maxl - 1 downto 0 do
    let tL = suffixL.(l) in
    cache.(l).(maxr) <- (metric (Some tL) None) + cache.(l+1).(maxr)
  done;
  for r = maxr - 1 downto 0 do
    let tR = suffixR.(r) in
    cache.(maxl).(r) <- (metric None (Some tR)) + cache.(maxl).(r+1)
  done;
  for l = maxl - 1 downto 0 do
    for r = maxr - 1 downto 0 do
      let tL, tR = suffixL.(l), suffixR.(r) in
      cache.(l).(r) <-
          minimum
            [ (metric None (Some tR)) + cache.(l).(r+1)
            ; (metric (Some tL) None) + cache.(l+1).(r)
            ; (metric (Some tL) (Some tR)) + cache.(l+1).(r+1) ]
  done done; cache.(0).(0)*)

let left_edit_distance suffixL suffixR =
(*   let suffixL, suffixR = suffixes forestL, suffixes forestR in *)
  let maxl, maxr = Array.length suffixL, Array.length suffixR in
  let cache = Array.make_matrix (maxl + 1) (maxr + 1) 0 in
  for l = maxl - 1 downto 0 do
(*     let tL = suffixL.(l) in *)
    cache.(l).(maxr) <- 5 + cache.(l+1).(maxr)
  done;
  for l = maxl - 1 downto 0 do
    for r = maxr - 1 downto 0 do
      let tL, tR = suffixL.(l), suffixR.(r) in
      cache.(l).(r) <-
          minimum
            [ 1 + cache.(l).(r+1)
            ; 5 + cache.(l+1).(r)
            ; (metric tL tR) + cache.(l+1).(r+1) ]
  done done; cache.(0).(0)

module CacheMap = Map.Make (
struct
  type t = (Latex.t * Latex.t)
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