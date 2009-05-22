module Http = Http_client.Convenience

let flush_line str = print_string str; print_string "\n"; flush stdout

(* Types and json parsing *)

type json document = < content : Latex.t >

and id = string

and ids = id list

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
    flush_line "Error contacting database (store/index)";
    raise Exit

let load_index () =
  try
    let attachment_url = store_url ^ "index/index" in
    (Marshal.from_string (Http.http_get attachment_url) 0 : index)
  with _ ->
    flush_line "Error contacting database (store/index)";
    raise Exit

let save_index index =
  try
    let revision = load_index_revision () in
    let attachment_url = store_url ^ "index/index?rev=" ^ revision in
    ignore (Http.http_put attachment_url (Marshal.to_string (index : index) [Marshal.No_sharing]))
  with _ ->
    flush_line "Error contacting database (store/index)";
    raise Exit

(* Database interaction *)

let db_url = "http://localhost:5984/documents/"

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
      then (flush_line "Received exit"; raise Exit)
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
  flush_line output

let handle_queries () =
  let bktree = (load_index ()).bktree in
  while true do
    let input = input_line stdin in
    handle_query bktree input
  done

(* Restarting the index search handler *)

let restart_index () =
  try
    ignore (Http.http_get (db_url ^ "_external/index?exit=true"));
    flush_line "Failed to restart the index search handler"
  with Http_client.Http_error (code,msg) ->
    if code = 500
    then flush_line "Restarted the index search handler"
    else flush_line "Failed to restart the search handler"

(* Updates *)

let batch_size = 5000

let get_update_batch last_update =
  let url =
    db_url ^
    "_all_docs_by_seq?include_docs=true" ^
    "&startkey=" ^ (string_of_int last_update) ^
    "&endkey=" ^ (string_of_int (last_update + batch_size)) in
  try
    let json = Json_io.json_of_string (Http.http_get url) in
    (updates_of_json json)#rows
  with _ ->
    flush_line "Error contacting database (documents)";
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
    flush_line ("Update failed for fragment: " ^ update#id ^ "");
    index

let run_update_batch batchsize index =
  flush_line ("Fetching next " ^ (string_of_int batchsize) ^ " updates");
  let updates = get_update_batch index.last_update in
  flush_line "Updating...";
  let index = List.fold_left run_update index updates in
  flush_line "Saving index";
  save_index index;
  index

let run_updates () =
  flush_line "Loading index";
  let index = load_index () in
  let rec run_update_batches index =
    let index' = run_update_batch 5000 index in
    if index.last_update != index'.last_update then run_update_batches index' else () in
  run_update_batches index;
  restart_index ()

(* Initialising index *)

let init_index () =
  flush_line "Creating index";
  save_index { last_update = 0 ; bktree = Bktree.Empty }

(* Main *)

open Arg
let _ = parse
  [("-init", Unit init_index, ": Create an empty index")
  ;("-update", Unit run_updates, ": Update the index")
  ;("-query", Unit handle_queries, ": Handle index queries as a couchdb _external")]
  ignore
  "Use 'index -help' for available options"
