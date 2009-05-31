type element =
  | Command of string
  | Text of string
and t = int array

let empty () = Array.create 0 0

exception Parse_error

(* Parsing elements from json *)
let of_json json =
  let rec element_of_json json =
    match json with
      | Json_type.Object [(command,json)] -> (Command command) :: t_of_json json
      | Json_type.String text -> [Text text]
      | _ -> raise Parse_error
  and t_of_json json =
    match json with
      | Json_type.Array jsons -> List.concat (List.map element_of_json jsons)
      | _ -> raise Parse_error in
  Array.of_list (List.map Hashtbl.hash (t_of_json json))

(* Defined to make json-static happy, not used *)
let to_json latex = Json_type.Null