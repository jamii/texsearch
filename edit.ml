open Util
open Latex

let rec minimum (x : int) y z =
  if y < x then minimum y x z else
  if z < y then minimum x z y else x

let left_edit_distance (suffixL : Latex.t) (suffixR : Latex.t) =
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
            (1 + cache.(l).(r+1))
            (1 + cache.(l+1).(r))
            ((abs (compare tL tR)) + cache.(l+1).(r+1))
  done done;
  (* Non-matches on the right dont count until left starts matching *)
  for r = maxr - 1 downto 0 do
    let tL, tR = suffixL.(0), suffixR.(r) in
      cache.(0).(r) <-
          minimum
            (cache.(0).(r+1))
            (1 + cache.(1).(r))
            ((abs (compare tL tR)) + cache.(1).(r+1))
  done;
  cache.(0).(0)