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

(* Parsing elements from json *)
let of_json json =
  let rec element_of_json json =
    match json with
      | Json_type.Object [(command,json)] -> (Command command) :: elements_of_json json
      | Json_type.String text -> [Text text]
      | _ -> raise Parse_error
  and elements_of_json json =
    match json with
      | Json_type.Array jsons -> List.concat (List.map element_of_json jsons)
      | _ -> raise Parse_error in
  Array.of_list (List.map Hashtbl.hash (elements_of_json json))

(* Defined to make json-static happy, not used *)
let to_json latex = Json_type.Null
