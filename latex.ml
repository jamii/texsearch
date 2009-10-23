(*
The internal representation of preprocessed latex strings.
The string elements are hashed to save space and speed up comparisons.
The json input is produced by the python preprocessor.
*)

type element =
  | Command of string
  | Text of string
and elements = int array

type t = elements

let empty () = Array.create 0 0

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
