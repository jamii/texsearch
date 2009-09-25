(*
Toplevel interaction with the search index.
Mostly I/O and error handling.
*)

module Http = Http_client.Convenience
let encode url = Netencoding.Url.encode ~plus:false url

let encode_doi doi = Str.global_replace (Str.regexp "/") "_" doi
let decode_doi doi = Str.global_replace (Str.regexp "_") "/" doi

let flush_line str = print_string str; print_string "\n"; flush stdout

(* Types and json parsing *)

type json doi = string

and id = string

and document =
  < journalID : string 
  ; publicationYear : string option
  ; content : (string * Latex.t) assoc (* id*Latex.t *)
  ; source : (string * string) assoc > (* id*string *)

and request =
  < args "query" :
    < searchTerm : string
    ; ?searchTimeout : string = "10.0"
    ; ?preprocessorTimeout : string = "5.0"
    ; ?limit : string = "1000"
    ; ?format : string = "xml" 
    ; ?doi : string option 
    ; ?journalID : string option
    ; ?publishedAfter : string option 
    ; ?publishedBefore : string option > >

and update =
  < id : doi
  ; key : int
  ; value : < ?deleted : bool = false >
  ; ?doc : Json_type.t option >

and updates =
  < rows : update list >

and result = doi * ((string * int) list)

and results = result list

and preprocessed =
  < json : Latex.t
  ; plain : string >

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

let preprocess timeout latex_string =
  let url = db_url ^ "_external/preprocess?format=json-plain&timeout=" ^ (encode timeout) ^ "&latex=" ^ (encode latex_string) in
  let preprocessed = preprocessed_of_json (Json_io.json_of_string (Http.http_get url)) in
  (preprocessed#json,preprocessed#plain)

(* Responses to couchdb *)

let json_response results query_string =
  Json_type.Object
    [ ("code",Json_type.Int 200)
    ; ("json",Json_type.Object
        [ ("query",Json_type.String query_string)
        ; ("results",json_of_results results) ]) ]

let json_limit_response = 
  Json_type.Object
    [ ("code",Json_type.Int 200)
    ; ("json",Json_type.String "Limit exceeded") ]

let xml_of_results results query_string =
  let xml_of_eqn (source,weight) =
    Xml.Element ("equation", [("distance",string_of_int weight)], [Xml.PCData source]) in
  let xml_of_result (doi,eqns) =
    Xml.Element ("result", 
      [("doi", decode_doi doi);("count", string_of_int (List.length eqns))], 
      (List.map xml_of_eqn eqns)) in
  let xml_of_query_string =
    Xml.Element ("query",[],[Xml.PCData query_string]) in
  Xml.to_string (Xml.Element ("results", [], xml_of_query_string :: (List.map xml_of_result results)))

let xml_response results query_string =
  Json_type.Object
    [ ("code",Json_type.Int 200)
    ; ("headers",Json_type.Object [("Content-type",Json_type.String "text/xml")])
    ; ("body",Json_type.String (xml_of_results results query_string)) ]

let xml_limit_response =
  Json_type.Object
    [ ("code",Json_type.Int 200)
    ; ("headers",Json_type.Object [("Content-type",Json_type.String "text/xml")])
    ; ("body",Json_type.String (Xml.to_string (Xml.Element ("LimitExceeded",[],[])))) ]

(* Timeouts *)

exception Timeout

let set_timer tsecs = ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.0; Unix.it_value = tsecs })

let with_timeout tsecs f =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
  try
    set_timer tsecs;
    let result = f () in
    set_timer 0.0;
    result
  with exc ->
    set_timer 0.0;
    raise exc

(* Queries *)

type search_result =
  | Results of results
  | LimitExceeded

let get_result filter (doi,ids) =
  let doc = get_document doi in
  if filter doc
  then 
    let weighted_source = List.map (fun (id,weight) -> (List.assoc id doc#source, weight)) ids in
    Some (decode_doi doi, weighted_source)
  else
    None

let run_single_query doi filter query =
  let eqns = (get_document doi)#content in
  let (_,weighted_eqns) = Bktree.query_against query eqns (Bktree.cutoff_length query) in
  Results (Util.filter_map (get_result filter) [(doi, weighted_eqns)])

let run_full_query bktree limit filter query =
  let results = Bktree.run_search (limit+1) (Bktree.new_search query bktree) in
  if List.length results > limit
  then LimitExceeded
  else Results (Util.filter_map (get_result filter) results)

let sort_results results =
  let weighted_results = 
    List.map 
      (fun (doi,eqns) -> 
        let weight = Util.minimum (List.map snd eqns) in
        let sorted_eqns = List.sort (fun a b -> compare (snd a) (snd b)) eqns in
        (weight,(doi,sorted_eqns)))
    results in
  let sorted_weighted_results = 
    List.sort (fun a b -> compare (fst a) (fst b)) weighted_results in
  List.map snd sorted_weighted_results

let handle_query bktree str =
  let response =
    try
      let args = (request_of_json (Json_io.json_of_string str))#args in
      let searchTimeout = float_of_string args#searchTimeout in
      let preprocessorTimeout = args#preprocessorTimeout in
      let limit = int_of_string args#limit in
      let query = Query.of_string (preprocess preprocessorTimeout) args#searchTerm in
      let filter document = 
            ((args#journalID = None) || (args#journalID = Some document#journalID))
        &&  ((args#publishedBefore = None) || ((args#publishedBefore >= document#publicationYear) && (document#publicationYear != None)))
        &&  ((args#publishedAfter  = None) || ((args#publishedAfter  <= document#publicationYear) && (document#publicationYear != None))) in
      let search_results = 
        with_timeout searchTimeout (fun _ ->
          match args#doi with
            | Some doi -> run_single_query (encode_doi doi) filter query (* Search within a single article *)
            | None -> run_full_query bktree limit filter query (* Search within all articles *)) in
      match (search_results, args#format) with
        | (Results results, "xml") -> xml_response (sort_results results) (Query.to_string query)
        | (LimitExceeded, "xml") -> xml_limit_response
        | (Results results, "json") -> json_response (sort_results results) (Query.to_string query)
        | (LimitExceeded, "json") -> json_limit_response
    with
      | Json_type.Json_error _ | Failure _ | Query.Parse_error ->
          Json_type.Object [("code",Json_type.Int 400)] (* Bad request *)
      | Timeout ->
          Json_type.Object [ ("code",Json_type.Int 500) (* Internal server error *)
                           ; ("headers",Json_type.Object [("Content-type",Json_type.String "text/plain")])
                           ; ("body",Json_type.String "Error: Timed out") ]
      | _ ->
          Json_type.Object [("code",Json_type.Int 500)] (* Internal server error *) in
  flush_line (Json_io.string_of_json ~compact:true response)

let handle_queries () =
  let bktree = (load_index ()).bktree in
  while true do
    let input = input_line stdin in
    handle_query bktree input
  done

(* Updating the index *)

let batch_size = 100

let get_update_batch last_update =
  flush_line
    ("Fetching updates from " ^
    (string_of_int (last_update+1)) ^
    " onwards");
  let url =
    db_url ^ "_all_docs_by_seq?include_docs=true" ^
    "&startkey=" ^ (string_of_int last_update) ^
    "&limit=" ^ (string_of_int batch_size) in
  try
    let json = Json_io.json_of_string (Http.http_get url) in
    (updates_of_json json)#rows
  with _ ->
    flush_line "Error contacting database (documents)";
    raise Exit

exception FailedUpdate of int * doi

let run_update index update =
  try
    let bktree = Bktree.delete update#id index.bktree in
    let bktree =
      match (update#value#deleted, update#doc) with
        | (_, None) | (true, _) -> bktree
        | (false, Some json) ->
            let doc = document_of_json json in
            if List.length doc#content > 0
            then 
              List.fold_left 
                (fun bktree eqn -> Bktree.add (Bktree.node_of update#id eqn) bktree)
                bktree
                doc#content
            else bktree in
    {last_update=update#key; bktree=bktree}
  with _ ->
    raise (FailedUpdate (update#key, update#id))

let rec run_update_batches index =
  let update_batch = get_update_batch index.last_update in
  flush_line "Updating...";
  let index = List.fold_left run_update index update_batch in
  flush_line "Saving index";
  save_index index;
  if List.length update_batch < batch_size then index else run_update_batches index

let run_updates () = 
  flush_line "Loading index";
  let index = load_index () in
  let index = 
    try
      run_update_batches index
    with FailedUpdate(key,id) ->
      flush_line ("Update " ^ (string_of_int key) ^ " failed (DOI: " ^ id ^ ")");
      index in
  flush_line ("Finished updating at update: " ^ (string_of_int index.last_update));
  flush_line "Ok"

(* Initialising index *)

let init_index () =
  print_string "This will erase the existing index. Are you sure? (y/n):"; flush stdout;
  if read_line () = "y"
  then
    (flush_line "Saving index";
     save_index { last_update = -1 ; bktree = Bktree.empty };
     flush_line "Ok")
  else
    flush_line "Ok, nothing was done"

(* Introspection *)

module DoiMap = Bktree.DoiMap

let list_all () =
  flush_line "Loading index";
  let index = load_index () in
  DoiMap.iter
    (fun doi eqns -> 
      flush_line ((decode_doi doi) ^ " (" ^ (string_of_int (List.length eqns)) ^ " equations)"))
    (Bktree.doi_map index.bktree)

let list_one doi =
  flush_line "Loading index";
  let index = load_index () in
  flush_line ("Searching for " ^ doi);
  try
    let eqns = DoiMap.find (encode_doi doi) (Bktree.doi_map index.bktree) in
    let journalID = (get_document (encode_doi doi))#journalID in
    flush_line ("DOI indexed with journal ID: " ^ journalID ^ " and equation ids:");
    List.iter (fun (id,_) -> flush_line id) eqns
  with Not_found ->
    flush_line "DOI not indexed"

(* Main *)

open Arg
let _ = parse
  [("-init", Unit init_index, ": Create an empty index")
  ;("-update", Unit run_updates, ": Update the index")
  ;("-query", Unit handle_queries, ": Handle index queries as a couchdb _external")
  ;("-list_all", Unit list_all, ": List all indexed keys")
  ;("-list", String list_one, ": List the entry for a given key")]
  ignore
  "Use 'index -help' for available options"
