module Http = Http_client.Convenience

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
    ; ?exit : string = "false" > >

and update =
  < id : id
  ; key : int
  ; value : < ?deleted : bool = false >
  ; ?doc : Json_type.t >

and updates =
  < rows : update list >

and revision =
  < rev "_rev" : string >

type index =
  { last_update : int
  ; bktree : Bktree.bktree }

(* Persisting *)

let store_url = "http://localhost:5984/store/"

let load_index_revision () =
  try
    let index_url = store_url ^ "index" in
    let json = Json_io.json_of_string (Http.http_get index_url) in
    (revision_of_json json)#rev
  with _ ->
    print_string "Error contacting database (store/index)\n";
    raise Exit

let load_index () =
  try
    let attachment_url = store_url ^ "index/attachment" in
    (Marshal.from_string (Http.http_get attachment_url) 0 : index)
  with _ ->
    print_string "Error contacting database (store/index)\n";
    raise Exit

let save_index index =
  try
    let revision = load_index_revision () in
    let attachment_url = store_url ^ "index/attachment?rev=" ^ revision in
    ignore (Http.http_put attachment_url (Marshal.to_string (index : index) [Marshal.No_sharing]))
  with _ ->
    print_string "Error contacting database (store/index)\n";
    raise Exit

(* Database interaction *)

let db_url = "http://localhost:5984/documents/"

let get_document id =
  let url = db_url ^ id in
  let json = Json_io.json_of_string (Http.http_get url) in
  (document_of_json json)#content

let get_all_documents () =
  let url = db_url ^ "_all_docs?include_docs=true" in
  try
    let json = Json_io.json_of_string (Http.http_get url) in
    (documents_of_json json)#rows
  with _ ->
    print_string "Error contacting database (documents)\n";
    raise Exit

(* Queries *)

let run_query bktree latex limit =
  match Bktree.next limit (Bktree.search latex bktree) with
    | Bktree.Last results -> List.map fst results
    | Bktree.More (results,_) -> List.map fst results

let handle_query bktree str =
  let response, code =
    try
      let query = (query_args_of_json (Json_io.json_of_string str))#query in
      if query#exit = "true"
      then (print_string "Received exit"; raise Exit)
      else
        let latex = Latex.of_json (Json_io.json_of_string query#latex) in
        let limit = int_of_string query#limit in
        json_of_ids (run_query bktree latex limit), Json_type.Int 200 (* OK *)
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
  let bktree = (load_index ()).bktree in
  while true do
    let input = input_line stdin in
    handle_query bktree input
  done

(* Restarting the index *)

let restart_index () =
  try
    ignore (Http.http_get (db_url ^ "_external/index?exit=true"));
    print_string "Failed to restart the index process\n"
  with Http_client.Http_error (code,msg) ->
    if code = 500
    then print_string "Restarted the index process\n"
    else print_string "Failed to restart the index process\n"

(* Updates *)

let get_updates last_update =
  let url = db_url ^ "_all_docs_by_seq?include_docs=true&startkey=" ^ (string_of_int last_update) in
  try
    let json = Json_io.json_of_string (Http.http_get url) in
    (updates_of_json json)#rows
  with _ ->
    print_string "Error contacting database (documents)\n";
    raise Exit

let run_update index update =
  try
    let bktree = Bktree.delete update#id index.bktree in
    let bktree =
      if update#value#deleted
      then bktree
      else Bktree.add (Bktree.node_of update#id (document_of_json update#doc)#content) bktree in
    {bktree=bktree; last_update=update#key}
  with _ ->
    print_string ("Update failed for document: " ^ update#id ^ "\n");
    index

let run_updates () =
  let index = load_index () in
  let index = List.fold_left run_update index (get_updates index.last_update) in
  save_index index;
  restart_index ()

(* Building *)

let get_last_update () =
  let url = db_url ^ "_all_docs_by_seq?descending=true&limit=1" in
  try
    let json = Json_io.json_of_string (Http.http_get url) in
    (List.hd (updates_of_json json)#rows)#key
  with _ ->
    print_string "Error contacting database (documents)\n";
    raise Exit

let build_index () =
  let last_update = get_last_update () in
  let docs = get_all_documents () in
  let add_doc bktree row =
    try
      let latex = (document_of_json row#doc)#content in
      Bktree.add (Bktree.node_of row#id latex) bktree
    with _ ->
      print_string ("Add failed for document: " ^ row#id ^ "\n");
      bktree in
  let bktree = List.fold_left add_doc Bktree.Empty docs in
  save_index { last_update = last_update ; bktree = bktree };
  restart_index ()

(* Main *)

open Arg
let _ = parse
  [("-rebuild", Unit build_index, ": Rebuild the index from scratch")
  ;("-update", Unit run_updates, ": Update the existing index")
  ;("-query", Unit handle_queries, ": Handle index queries as a couchdb _external")]
  ignore
  "Use 'index -help' for available options"
