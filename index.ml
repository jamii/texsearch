exception Bad_request

(* Types and json parsing *)

type json document = < content : Latex.t >

and id = string

and ids = id list

and documents =
  < rows : < id : id ; doc : Json_type.t > list >

and query_args =
  < query :
    < ?latex : string = "[]"
    ; ?limit : string = "10"
    ; ?exit : bool = false > >

and update =
  < id : id
  ; key : int
  ; value : < ?deleted : bool = false >
  ; ?doc : Json_type.t >

and updates =
  < rows : update list >

type index =
  { last_update : int
  ; mtree : Mtree.mtree }

(* Persisting *)

let load_index () =
  let index_file = open_in_bin "/home/jamie/texsearch/index_store" in
  let index = (Marshal.from_channel index_file : index) in
  close_in index_file; index

let save_index index =
  let index_file = open_out_bin "/home/jamie/texsearch/index_store" in
  Marshal.to_channel index_file index [Marshal.No_sharing];
  close_out index_file

(* Database interaction *)

let db_url = "http://localhost:5984/documents/"

let get_document id =
  let url = db_url ^ id in
  let json = Json_io.json_of_string (Http_client.Convenience.http_get url) in
  (document_of_json json)#content

let get_all_documents () =
  let url = db_url ^ "_all_docs?include_docs=true" in
  try
    let json = Json_io.json_of_string (Http_client.Convenience.http_get url) in
    (documents_of_json json)#rows
  with _ ->
    print_string "Error contacting database\n";
    raise Exit

(* Queries *)

let run_query mtree latex limit =
  match Mtree.next limit (Mtree.search latex mtree) with
    | Mtree.Last results -> List.map fst results
    | Mtree.More (results,_) -> List.map fst results

let handle_query mtree str =
  let response, code =
    try
      let query = (query_args_of_json (Json_io.json_of_string str))#query in
      if query#exit
      then (print_string "Received exit"; raise Exit)
      else
        let latex = Latex.of_json (Json_io.json_of_string query#latex) in
        let limit = int_of_string query#limit in
        json_of_ids (run_query mtree latex limit), Json_type.Int 200 (* OK *)
    with
      | Json_type.Json_error _ | Latex.Bad_latex | Failure _ -> Json_type.Null, Json_type.Int 400 (* Bad request *)
      | _ -> Json_type.Null, Json_type.Int 500 (* Internal server error *) in
  let output =
    Json_io.string_of_json ~compact:true
      (Json_type.Object
        [ ("code",code)
        ; ("json",response) ]) in
  print_string (output ^ "\n"); flush stdout

let handle_queries () =
  let mtree =
    try (load_index ()).mtree
    with _ ->
      print_string "Could not open index!\n";
      raise Exit in
  while true do
    let input = input_line stdin in
    handle_query mtree input
  done

(* Updates *)

let get_updates last_update =
  let url = db_url ^ "_all_docs_by_seq?include_docs=true&startkey=" ^ (string_of_int last_update) in
  try
    let json = Json_io.json_of_string (Http_client.Convenience.http_get url) in
    (updates_of_json json)#rows
  with _ ->
    print_string "Error contacting database\n";
    raise Exit

let run_update index update =
  try
    let mtree = Mtree.delete update#id index.mtree in
    let mtree =
      if update#value#deleted
      then mtree
      else Mtree.add (Mtree.node_of update#id (document_of_json update#doc)#content) mtree in
    {mtree=mtree; last_update=update#key}
  with _ ->
    print_string ("Update failed for document: " ^ update#id ^ "\n");
    index

let run_updates () =
  let index = load_index () in
  let index = List.fold_left run_update index (get_updates index.last_update) in
  save_index index

(* Building *)

let get_last_update () =
  let url = db_url ^ "_all_docs_by_seq?descending=true&limit=1" in
  try
    let json = Json_io.json_of_string (Http_client.Convenience.http_get url) in
    (List.hd (updates_of_json json)#rows)#key
  with _ ->
    print_string "Error contacting database\n";
    raise Exit

let build_index () =
  let last_update = get_last_update () in
  let docs = get_all_documents () in
  let add_doc mtree row =
    try
      let latex = (document_of_json row#doc)#content in
      Mtree.add (Mtree.node_of row#id latex) mtree
    with _ ->
      print_string ("Add failed for document: " ^ row#id ^ "\n");
      mtree in
  let mtree = List.fold_left add_doc Mtree.Empty docs in
  save_index { last_update = last_update ; mtree = mtree }

(* Main *)

open Arg
let _ = parse
  [("-rebuild", Unit build_index, ": Rebuild the index from scratch")
  ;("-update", Unit run_updates, ": Update the existing index")
  ;("-query", Unit handle_queries, ": Handle mtree queries as a couchdb _external")]
  ignore
  "Use 'mtree -help' for available options"
