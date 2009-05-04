exception Bad_request

let index = ref Mtree.empty

(* Types and json parsing *)

type json document = < content : Latex.t >

and id = string

and ids = id list

and documents =
  < rows : < id : id ; doc : Json_type.t > list >

and query_args =
  < query :
    < latex : string
    ; limit : string
    ; cutoff : string > >

and update =
  < typ "type" : string
  ; id : string
  ; doc "new" : document option >

exception Fixme

(* Database interaction *)

let db_url = "http://localhost:5984/documents/"

let get_document id =
  let url = db_url ^ id in
  try
    let json = Json_io.json_of_string (Http_client.Convenience.http_get url) in
    Some (document_of_json json)#content
  with _ -> None

let get_all_documents () =
  let url = db_url ^ "_all_docs?include_docs=true" in
  let json = Json_io.json_of_string (Http_client.Convenience.http_get url) in
  (documents_of_json json)#rows

(* Query handler *)

let run_query latex limit cutoff =
  match Mtree.next limit cutoff (Mtree.search latex !index) with
    | Mtree.Last results -> List.map fst results
    | Mtree.More (results,_) -> List.map fst results

let handle_query str =
  let response, code =
    (*try*)
      let query = (query_args_of_json (Json_io.json_of_string str))#query in
      let latex = Latex.of_json (Json_io.json_of_string query#latex) in
      let limit = int_of_string query#limit in
      let cutoff = float_of_string query#cutoff in
      json_of_ids (run_query latex limit cutoff), Json_type.Int 200 (* OK *)
    (*with
      | Json_type.Json_error _ | Latex.Bad_latex | Failure _ -> Json_type.Null, Json_type.Int 400 (* Bad request *)
      | _ -> Json_type.Null, Json_type.Int 500 (* Internal server error *)*) in
  let output =
    Json_io.string_of_json ~compact:true
      (Json_type.Object
        [ ("code",code)
        ; ("json",response) ]) in
  print_string (output ^ "\n"); flush stdout

(* Update handling *)

let handle_update str =
  try
    let update = (update_of_json (Json_io.json_of_string str)) in
    match update#typ with
      | "create" ->
          (match update#doc with
            | Some doc ->
                index := Mtree.add (Mtree.node update#id doc#content) !index)
      | "delete" ->
          index := Mtree.delete update#id !index
      | "update" ->
          (match update#doc with
            | Some doc ->
                index := Mtree.add (Mtree.node update#id doc#content) (Mtree.delete update#id !index))
  with _ -> raise Fixme

(*  *)

let init_index () =
  let docs =
    (*try *)get_all_documents ()
    (*with _ -> raise Fixme*) in
  let add_doc index row =
    try
      let doc = document_of_json row#doc in
      Mtree.add (Mtree.node row#id doc#content) index
    with _ -> index in
  List.fold_left add_doc Mtree.empty docs

let run_handlers () =
  index := init_index ();
  let notify = Unix.openfile "/home/jamie/texsearch/notify" [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666 in
  let handle file =
    let input = input_line (Unix.in_channel_of_descr file) in
    if file = Unix.stdin
    then handle_query input
    else handle_update input in
  while true do
    let (files,_,_) = Unix.select [notify;Unix.stdin] [] [] (-1.0) in
    List.iter handle files
  done