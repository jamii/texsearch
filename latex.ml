type element =
  | Command of int
  | Text of int
and t = element array

(* Parsing elements from json *)
exception Bad_latex

let of_json json =
  let rec element_of_json json =
    match json with
      | Json_type.Object [(command,json)] -> (Command (Hashtbl.hash command)) :: t_of_json json
      | Json_type.String str -> [Text (Hashtbl.hash str)]
      | _ -> raise Bad_latex
  and t_of_json json =
    match json with
      | Json_type.Array jsons -> List.concat (List.map element_of_json jsons)
      | _ -> raise Bad_latex in
  Array.of_list (t_of_json json)

(* Defined to make json-static happy, not used *)
let to_json latex = Json_type.Null