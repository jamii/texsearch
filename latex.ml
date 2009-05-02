type element =
  | Command of string * fragment
  | Text of string
and fragment = element list

let children element =
  match element with
    | Command (_,fragment) -> fragment
    | Text _ -> []

(* Parsing from xml *)
exception Bad_xml

let rec element_of_xml xml =
  match xml with
    | Xml.Element (tag,attrs,xml) -> Command (tag, fragment_of_xml xml)
    | Xml.PCData text -> Text text

and fragment_of_xml xmls = List.map element_of_xml xmls

let fragment_of_document doc =
  match doc with
    | Xml.Element ("document",_,contents) -> fragment_of_xml contents

let parse_xml str =
  try
    let xml = Xml.parse_string str in
    match xml with
      | Xml.Element ("results",_,results) ->
          List.map fragment_of_document results
  with
    | Xml.Error _ | Match_failure _ -> raise Bad_xml

(* Parsing elements from json *)
exception Bad_latex

let rec element_of_json json =
  match json with
    | Json_type.Object ((field,json)::rest) -> Command (field,fragment_of_json json)
    | Json_type.String str -> Text str
    | _ -> raise Bad_latex
and fragment_of_json json =
  match json with
    | Json_type.Array jsons -> List.map element_of_json jsons
    | _ -> raise Bad_latex