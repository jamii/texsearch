(*
Calculation of the Levensthein edit distance between two latex strings.
The calculation is left-biased: the left string is matched to any substring of the right string
*)

open Latex

let rec minimum (x : int) y z =
  if y < x then minimum y x z else
  if z < y then minimum x z y else x

(* Number of edits required to match latexL to a substring of latexR *)
let left_edit_distance (latexL : Latex.t) (latexR : Latex.t) =
  let maxl, maxr = Array.length latexL, Array.length latexR in
  if maxl = 0 then max_int else (* horrible hack, dont want empty strings to match anything *)
  if maxr = 0 then maxl else
  (* cache.(l).(r) is the left_edit_distance between latexL[l to maxl] and latexR[r to maxr] *)
  let cache = Array.make_matrix (maxl + 1) (maxr + 1) 0 in
  (* Must match everything on the left *)
  for l = maxl - 1 downto 0 do
    cache.(l).(maxr) <- 1 + cache.(l+1).(maxr)
  done;
  (* General matching *)
  for l = maxl - 1 downto 1 do
    for r = maxr - 1 downto 0 do
      cache.(l).(r) <-
          minimum
            (1 + cache.(l).(r+1))
            (1 + cache.(l+1).(r))
            ((abs (compare latexL.(l) latexR.(r))) + cache.(l+1).(r+1))
  done done;
  (* Non-matches on the right dont count until left starts matching *)
  for r = maxr - 1 downto 0 do
    cache.(0).(r) <-
        minimum
          (cache.(0).(r+1))
          (1 + cache.(1).(r))
          ((abs (compare latexL.(0) latexR.(r))) + cache.(1).(r+1))
  done;
  cache.(0).(0)
