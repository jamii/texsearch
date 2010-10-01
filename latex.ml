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

(* Parsing elements from json, splitting at line boundaries *)
let lines_of_json json =
  let rec lines line el =
    match el with
      | [] -> [line]
      | ((Command "\\")::el) | ((Command "ArrayCell")::el) -> (List.rev line) :: (lines [] el)
      | (e::el) -> lines (e::line) el in
  List.map 
    (fun line -> Array.of_list (List.map Hashtbl.hash line)) 
    (lines [] (element_list_of_json json))

(* Defined to make json-static happy, not used *)
let to_json latex = Json_type.Null

let length = Array.length

type pos = int

let compare_suffix (latex1, pos1) (latex2, pos2) =
  let n1, n2 = length latex1, length latex2 in
  let rec compare_suffix' pos1 pos2 =
    match (pos1 >= n1, pos2 >= n2) with
    | (true, true) -> 0
    | (true, false) -> -1
    | (false, true) ->  1
    | (false, false) ->
	let cmp = compare latex1.(pos1) latex2.(pos2) in
	if cmp < 0 then -1 else
	if cmp > 0 then  1 else
	compare_suffix' (pos1+1) (pos2+1) in
  compare_suffix' pos1 pos2

let is_prefix (latex1, pos1) (latex2, pos2) =
  let n1, n2 = length latex1, length latex2 in
  let rec is_prefix' pos1 pos2 =
    if pos1 >= n1 then true else
    if pos2 >= n2 then false else
    if latex1.(pos1) != latex2.(pos2) then false else
    is_prefix' (pos1+1) (pos2+1) in
  is_prefix' pos1 pos2

(* Divide latex into k equal(ish) substrings *)
let fragments latex k =
  let n = length latex in
  let size = n / k in
  let rec fragments' pos larger =
    if pos >= n then [] else
    let size = if larger > 0 then size+1 else size in
    (Array.sub latex pos size) :: (fragments' (pos+size) (larger-1)) in
  fragments' 0 (n mod k)
