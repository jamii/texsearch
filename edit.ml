open Util
open Latex

let metric a b =
  match (a,b) with
    | (Text ta, Text tb) -> if ta = tb then 0 else 1
    | (Command (ca,_), Command (cb,_)) -> if ca = cb then 0 else 1
    | _ -> 1

let suffixes forest =
  let rec loop forest =
    match forest with
      | [] -> []
      | (t::ts) -> t :: (loop ((children t) @ ts)) in
  Array.of_list (loop forest)

let left_edit_distance suffixL suffixR =
(*   let suffixL, suffixR = suffixes forestL, suffixes forestR in *)
  let maxl, maxr = Array.length suffixL, Array.length suffixR in
  if maxl = 0 then maxr else
  if maxr = 0 then maxl else
  let cache = Array.make_matrix (maxl + 1) (maxr + 1) 0 in
  (* Must match everything on the left *)
  for l = maxl - 1 downto 0 do
    cache.(l).(maxr) <- 1 + cache.(l+1).(maxr)
  done;
  (* General matching *)
  for l = maxl - 1 downto 1 do
    for r = maxr - 1 downto 0 do
      let tL, tR = suffixL.(l), suffixR.(r) in
      cache.(l).(r) <-
          minimum
            [ 1 + cache.(l).(r+1)
            ; 1 + cache.(l+1).(r)
            ; (metric tL tR) + cache.(l+1).(r+1) ]
  done done;
  (* Non-matches on the right dont count until left starts matching *)
  for r = maxr - 1 downto 0 do
    let tL, tR = suffixL.(0), suffixR.(r) in
      cache.(0).(r) <-
          minimum
            [ cache.(0).(r+1)
            ; 1 + cache.(1).(r)
            ; (metric tL tR) + cache.(1).(r+1) ]
  done;
  cache.(0).(0)

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