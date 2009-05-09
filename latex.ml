type element =
  | Command of string
  | Text of string
and t = element array

(* Parsing elements from json *)
exception Bad_latex

let of_json json =
  let rec element_of_json json =
    match json with
      | Json_type.Object [(field,json)] -> (Command field) :: t_of_json json
      | Json_type.String str -> [Text str]
      | _ -> raise Bad_latex
  and t_of_json json =
    match json with
      | Json_type.Array jsons -> List.concat (List.map element_of_json jsons)
      | _ -> raise Bad_latex in
  Array.of_list (t_of_json json)

(* Defined to make json-static happy, not used *)
let to_json latex = Json_type.Null