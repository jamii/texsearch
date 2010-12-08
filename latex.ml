(*
The internal representation of preprocessed latex strings.
The string elements are hashed to save space and speed up comparisons.
The json input is produced by the python preprocessor.
*)

type element =
  | Command of string
  | Text of string

type t = int array

let empty () = Array.make 0 0

let of_array array = array

exception Parse_error

let rec element_of_json json =
  match json with
    | Json_type.Object [(command,json)] -> (Command command) :: element_list_of_json json
    | Json_type.String text -> [Text text]
    | _ -> raise Parse_error
and element_list_of_json json =
  match json with
    | Json_type.Array jsons -> List.concat (List.map element_of_json jsons)
    | _ -> raise Parse_error

(* Parsing elements from json *)
let of_json json =
  Array.of_list (List.map Hashtbl.hash (element_list_of_json json))

(* Defined to make json-static happy, not used *)
let to_json latex = Json_type.Null

let length = Array.length

type pos = int

let compare_suffix (latexL, pos1) (latexR, pos2) =
  let n1, n2 = length latexL, length latexR in
  let rec compare_suffix' pos1 pos2 =
    match (pos1 >= n1, pos2 >= n2) with
    | (true, true) -> 0
    | (true, false) -> -1
    | (false, true) ->  1
    | (false, false) ->
	let cmp = compare latexL.(pos1) latexR.(pos2) in
	if cmp < 0 then -1 else
	if cmp > 0 then  1 else
	compare_suffix' (pos1+1) (pos2+1) in
  compare_suffix' pos1 pos2

let is_prefix (latexL, pos1) (latexR, pos2) =
  let n1, n2 = length latexL, length latexR in
  let rec is_prefix' pos1 pos2 =
    if pos1 >= n1 then true else
    if pos2 >= n2 then false else
    if latexL.(pos1) != latexR.(pos2) then false else
    is_prefix' (pos1+1) (pos2+1) in
  is_prefix' pos1 pos2

(* Divide latex into k substrings of equal(ish) lengths *)
let fragments latex k =
  let n = length latex in
  let size = n / k in
  let rec fragments' pos larger =
    if pos >= n then [] else
    let size = if larger > 0 then size+1 else size in
    (Array.sub latex pos size) :: (fragments' (pos+size) (larger-1)) in
  fragments' 0 (n mod k)

let rec minimum (x : int) y z =
  if y < x then minimum y x z else
  if z < y then minimum x z y else x

let cutoff precision latex =
  let errors = (1.0 -. precision) *. (float_of_int (length latex)) in
  max 1 (min 5 (int_of_float (ceil errors)))

(*
Calculation of the Levensthein edit distance between two latex strings.
The calculation is left-biased: the left string is matched to any substring of the right string
*)
let distance latexL latexR =
  let maxl, maxr = Array.length latexL, Array.length latexR in
  if maxl = 0 then max_int else (* horrible hack, dont want empty strings to match anything *)
  if maxr = 0 then maxl else
  (* cache.(l).(r) is the distance between latexL[l to maxl] and latexR[r to maxr] *)
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

let similar precision latexL latexR =
  let dist = distance latexL latexR in
  if dist < cutoff precision latexL then Some dist else None
