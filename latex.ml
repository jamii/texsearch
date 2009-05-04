type element =
  | Command of string * t
  | Text of string
and t = element list

let children element =
  match element with
    | Command (_,latex) -> latex
    | Text _ -> []

(* Parsing elements from json *)
exception Bad_latex

let rec element_of_json json =
  match json with
    | Json_type.Object [(field,json)] -> Command (field,of_json json)
    | Json_type.String str -> Text str
    | _ -> raise Bad_latex
and of_json json =
  match json with
    | Json_type.Array jsons -> List.map element_of_json jsons
    | _ -> raise Bad_latex

(* Defined to make json-static happy, not used *)
let to_json latex = Json_type.Null