(*
Toplevel interaction with the search index.
This mostly consists of I/O and error handling.
*)

module Http = Http_client.Convenience
let encode url = Netencoding.Url.encode ~plus:false url

let decodeDoi doi = Str.global_replace (Str.regexp "_") "/" doi

let flush_line str = print_string str; print_string "\n"; flush stdout

(* Types and json parsing *)

type json doi = string

and id = string

and document =
  < content : (string * Latex.t) assoc (* id*Latex.t *)
  ; source : (string * string) assoc > (* id*string *)

and request =
  < args "query" :
    < searchTerm : string
    ; ?startAt : string option
    ; ?endAt : string option
    ; ?format : string = "xml" > >

and update =
  < id : doi
  ; key : int
  ; value : < ?deleted : bool = false >
  ; doc : Json_type.t >

and updates =
  < rows : update list >

and result = doi * (string list)

and results = result list

type index =
  { last_update : int
  ; bktree : Bktree.bktree }

(* Persisting *)

let load_index () =
  try
    let index_file = open_in_bin "/opt/texsearch/index_store" in
    let index = (Marshal.from_channel index_file : index) in
    close_in index_file; index
  with _ ->
    flush_line "Could not open file /opt/texsearch/index_store";
    raise Exit

let save_index index =
  try
    let index_file = open_out_bin "/opt/texsearch/index_store_tmp" in
    Marshal.to_channel index_file index [Marshal.No_sharing];
    close_out index_file;
    Unix.rename "/opt/texsearch/index_store_tmp" "/opt/texsearch/index_store"
  with _ ->
    flush_line "Could not save to file /opt/texsearch/index_store";
    raise Exit

(* Database interaction *)

let db_url = "http://localhost:5984/documents/"

let get_document doi =
  let url = db_url ^ doi in
  let json = Json_io.json_of_string (Http.http_get url) in
  document_of_json json

let get_result (doi,ids) =
  let source = (get_document doi)#source in
  (decodeDoi doi, List.map (fun id -> List.assoc id source) ids)

let preprocess latex_string =
  let url = db_url ^ "_external/preprocess?latex=" ^ (encode latex_string) in
  let latex_json = Http.http_get url in
  Latex.of_json (Json_io.json_of_string latex_json)

(* Responses to couchdb *)

type response =
  | Ok of Json_type.t
  | BadRequest
  | InternalServerError

let json_response results =
  Json_type.Object
    [ ("code",Json_type.Int 200)
    ; ("json",json_of_results results) ]

let xml_of_results results =
  let xml_of_source source =
    Xml.Element ("Equation", [], [Xml.PCData source]) in
  let xml_of_result (doi,sources) =
    Xml.Element ("Result", [("doi", decodeDoi doi)], (List.map xml_of_source sources)) in
  Xml.to_string (Xml.Element ("Results", [], (List.map xml_of_result results)))

let xml_response results =
  Json_type.Object
    [ ("code",Json_type.Int 200)
    ; ("headers",Json_type.Object [("Content-type",Json_type.String "text/xml")])
    ; ("body",Json_type.String (xml_of_results results)) ]

(* Queries *)

let run_query bktree query startAt endAt =
  List.map get_result (Util.drop startAt (Bktree.run_search endAt (Bktree.new_search query bktree)))

let handle_query bktree str =
  let response =
    (*try*)
      let args = (request_of_json (Json_io.json_of_string str))#args in
      let query = Query.of_string preprocess args#searchTerm in
      let startAt =
        match args#startAt with
          | None -> 0
          | Some str -> int_of_string str in
      let endAt =
        match args#endAt with
          | None -> max_int
          | Some str -> int_of_string str in
      let results = run_query bktree query startAt endAt in
      match args#format with
        | "xml" -> xml_response results
        | "json" -> json_response results in
(*    with
      | Json_type.Json_error _ | Failure _ | Query.Parse_error ->
          Json_type.Object [("code",Json_type.Int 400)] (* Bad request *)
      | _ ->
          Json_type.Object [("code",Json_type.Int 500)] in (* Internal server error *)*)
  flush_line (Json_io.string_of_json ~compact:true response)

let handle_queries () =
  let bktree = (load_index ()).bktree in
  while true do
    let input = input_line stdin in
    handle_query bktree input
  done

(* Updates *)

let batch_size = 100

let get_update_batch last_update =
  let url =
    db_url ^ "_all_docs_by_seq?include_docs=true" ^
    "&startkey=" ^ (string_of_int (last_update + 1)) ^
    "&endkey=" ^ (string_of_int (last_update + batch_size)) in
  try
    let json = Json_io.json_of_string (Http.http_get url) in
    (updates_of_json json)#rows
  with _ ->
    flush_line "Error contacting database (documents)";
    raise Exit

let run_update index update =
  try
    Bktree.delete update#id index.bktree;
    if not (update#value#deleted)
    then
      let doc = document_of_json update#doc in
      Bktree.add (Bktree.node_of update#id doc#content) index.bktree
    else ();
    {index with last_update=update#key}
  with _ ->
    flush_line ("Update failed for document: " ^ update#id ^ "");
    index

let run_update_batch index =
  flush_line
    ("Fetching updates " ^
    (string_of_int index.last_update) ^
    " through " ^
    (string_of_int (index.last_update + batch_size)));
  let updates = get_update_batch index.last_update in
  flush_line "Updating...";
  let index = List.fold_left run_update index updates in
  flush_line "Saving index";
  save_index index;
  load_index ()

let run_updates () =
  flush_line "Loading index";
  let index = load_index () in
  let rec run_update_batches index =
    let index' = run_update_batch index in
    if index.last_update != index'.last_update then run_update_batches index' else () in
  run_update_batches index;
  flush_line ("Finished updating at update: " ^ (string_of_int index.last_update));
  flush_line "Ok"

(* Initialising index *)

let init_index () =
  print_string "This will erase the existing index. Are you sure? (y/n):"; flush stdout;
  if read_line () = "y"
  then
    (flush_line "Saving index";
     save_index { last_update = 0 ; bktree = Bktree.empty };
     flush_line "Ok")
  else
    flush_line "Ok, nothing was done"

(* Main *)

open Arg
  let _ = parse
  [("-init", Unit init_index, ": Create an empty index")
  ;("-update", Unit run_updates, ": Update the index")
  ;("-query", Unit handle_queries, ": Handle index queries as a couchdb _external")]
  ignore
  "Use 'index -help' for available options"
