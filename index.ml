(*
This module controls toplevel interaction with the search index.
Mostly I/O and error handling. See the last section for the commands supported.
*)

(* Types and json parsing *)

type json doi = string

and eqnID = string
and journalID = string
and publicationYear = string

and document =
  < journalID : journalID
  ; publicationYear : publicationYear option
  ; content : (string * Latex.t) assoc (* eqnID*Latex.t *)
  ; source : (string * string) assoc > (* eqnID*string *)

and request =
  < args "query" :
    < searchTerm : string
    ; ?searchTimeout : string = "10.0"
    ; ?preprocessorTimeout : string = "5.0"
    ; ?limit : string = "1000"
    ; ?doi : string option 
    ; ?journalID : journalID option
    ; ?publishedAfter : publicationYear option
    ; ?publishedBefore : publicationYear option > >

and update =
  < id : doi
  ; key : int
  ; value : < ?deleted : bool = false >
  ; ?doc : Json_type.t option >

and updates =
  < rows : update list >

and preprocessed =
  < json : Latex.t
  ; plain : string >

type eqn_node = 
  { doi : doi
  ; eqnID : eqnID
  ; latex : Latex.t }

(* Assorted imports and utililty functions *)

module Http = Http_client.Convenience
let encode url = Netencoding.Url.encode ~plus:false url

(* couchdb does not allow '/' in keys *)
let encode_doi doi = Str.global_replace (Str.regexp "/") "_" doi
let decode_doi doi = Str.global_replace (Str.regexp "_") "/" doi

let flush_line str = print_string str; print_string "\n"; flush stdout

module Doi_map = 
struct
  include Map.Make (struct
    type t = doi
    let compare = compare
  end)

  let update key f default map =
    add key (try f (find key map) with Not_found -> default) map

  let count map = fold (fun _ _ n -> n+1) map 0

  let list_of map = fold (fun k v rest -> (k,v) :: rest) map []
end

(* Our main index structure *)

module Eqn_node_metric : (
sig 
  type t = eqn_node
  val dist : eqn_node -> eqn_node -> int
end) =
struct
  type t = eqn_node
  let dist a b = Edit.left_edit_distance a.latex b.latex
end

module Index_tree = Bktree.Make (Eqn_node_metric)

type index =
  { last_update : int (* Key of the last update received from couchdb *)
  ; index_tree : Index_tree.t
  ; metadata : (journalID * publicationYear option * int) Doi_map.t }

(* Persisting *)

let load_index () =
  try
    let index_file = open_in_bin "./index_store" in
    let index = (Marshal.from_channel index_file : index) in
    close_in index_file; index
  with _ ->
    flush_line "Error opening file ./index_store";
    raise Exit

let save_index index =
  try
    let index_file = open_out_bin "./index_store_tmp" in
    Marshal.to_channel index_file index [Marshal.No_sharing];
    close_out index_file;
    Unix.rename "./index_store_tmp" "./index_store"
  with _ ->
    flush_line "Error saving to file ./index_store";
    raise Exit

(* Database interaction *)

let couchdb_url = 
  (* Ocaml's file handling is terrible... *)
  let conf = open_in "./db.ini" in
  let rec read_port () =
    try
      let line = input_line conf in
      Str.search_forward (Str.regexp "port *= *\([0-9]+\)") line 0;
      Str.matched_group 1 line
    with Not_found -> read_port () in
  "http://localhost:" ^ read_port () ^ "/"

let db_url = couchdb_url ^ "documents/"

let get_document doi =
  let url = db_url ^ doi in
  let json = Json_io.json_of_string (Http.http_get url) in
  document_of_json json

let preprocess timeout latex_string =
  let url = db_url ^ "_external/preprocess?format=json-plain&timeout=" ^ (encode timeout) ^ "&latex=" ^ (encode latex_string) in
  let preprocessed = preprocessed_of_json (Json_io.json_of_string (Http.http_get url)) in
  (preprocessed#json,preprocessed#plain)

(* Responses to couchdb *)

let xml_of_results results query_string =
  let xml_of_eqn (eqnID,weight) =
    Xml.Element ("equation", [("distance",string_of_int weight);("id",eqnID)], []) in
  let xml_of_result (doi,eqns) =
    Xml.Element ("result", 
      [("doi", decode_doi doi);("count", string_of_int (List.length eqns))], 
      (List.map xml_of_eqn eqns)) in
  let xml_of_query_string =
    Xml.Element ("query",[],[Xml.PCData query_string]) in
  Xml.Element ("results", [], xml_of_query_string :: (List.map xml_of_result results))

let xml_error error = Xml.Element (error,[],[])

let xml_response xml = 
  Json_type.Object
    [ ("code",Json_type.Int 200)
    ; ("headers",Json_type.Object [("Content-type",Json_type.String "text/xml")])
    ; ("body",Json_type.String (Xml.to_string xml)) ]

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

(* The choice of cutoff is completely arbitrary *)
let cutoff_length query = 1 + (min 3 (Query.max_length query / 3))

let run_query index query filter limit =
  let eqns = 
    Util.list_of_stream 
    (Index_tree.run_query 
      (fun eqn_node -> Query.query_dist query eqn_node.latex) 
      (cutoff_length query) 
      index.index_tree) in
  (* Collate eqns by doi *)
  let doi_map =
    List.fold_left
      (fun doi_map (eqn_node,weight) -> 
        let (key, value) = (eqn_node.doi, (eqn_node.eqnID,weight)) in
        Doi_map.update key (fun values -> value::values) [value] doi_map)
      Doi_map.empty      
      eqns in
  (* Remove the dummy node *)
  let doi_map = Doi_map.remove "" doi_map in
  if Doi_map.count doi_map > limit 
  then 
    xml_error "LimitExceeded"
  else
    let results = Doi_map.list_of doi_map in
    let results = List.filter (fun (doi,_) -> filter doi) results in
    (* Sort each set of equations by weight *)
    let results = List.map (fun (doi,eqns) -> (doi,List.sort (fun a b -> compare (snd a) (snd b)) eqns)) results in
    (* Sort doi's by lowest weighted equation *)
    let results = List.sort (fun (_,eqnsA) (_,eqnsB) -> compare (snd (List.hd eqnsA)) (snd (List.hd eqnsB))) results in
    xml_of_results results (Query.to_string query)

let handle_query index str =
  try
    let args = (request_of_json (Json_io.json_of_string str))#args in
    let searchTimeout = float_of_string args#searchTimeout in
    let preprocessorTimeout = args#preprocessorTimeout in
    let limit = int_of_string args#limit in
    let query = Query.of_string (preprocess preprocessorTimeout) args#searchTerm in
    let filter doi = 
      let (journalID, publicationYear, _) = Doi_map.find doi index.metadata in
          ((args#journalID = None) || (args#journalID = Some journalID))
      &&  ((args#publishedBefore = None) || ((args#publishedBefore >= publicationYear) && (publicationYear != None)))
      &&  ((args#publishedAfter  = None) || ((args#publishedAfter  <= publicationYear) && (publicationYear != None))) in
    xml_response (with_timeout searchTimeout (fun () -> run_query index query filter limit))
  with
    | Json_type.Json_error _ | Failure _ -> xml_response (xml_error "ArgParseError")
    | Query.Parse_error -> xml_response (xml_error "QueryParseError")
    | Timeout -> xml_response (xml_error "TimedOut")
    | _ -> Json_type.Object [("code",Json_type.Int 500)] (* Internal server error *)

let handle_queries () =
  let index = load_index () in
  while true do
    let input = input_line stdin in
    let json = handle_query index input in
    flush_line (Json_io.string_of_json ~compact:true json)
  done

(* Initialising index *)

let init_index () =
  flush_line ("couchdb is at " ^ couchdb_url);
  print_string "This will erase the existing index. Are you sure? (y/n):"; flush stdout;
  if read_line () = "y"
  then
    (flush_line "Saving index";
    (* We stick a dummy node at the base of the tree so we don't have to deal with empty trees *)
    let index_tree = Index_tree.empty_branch {doi=""; eqnID=""; latex=Latex.empty()} in
    save_index {last_update = -1; index_tree = index_tree; metadata = Doi_map.empty};
    flush_line "Ok")
  else
    flush_line "Ok, nothing was done"

(* Updating the index *)

let batch_size = 1000

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
    (* Start by deleting old version of the document if it already exists *)
    let index = 
      if not (Doi_map.mem update#id index.metadata) then index else
      let index_tree = 
        match Index_tree.filter (fun eqn_node -> eqn_node.doi != update#id) index.index_tree with
          | Some index_tree -> index_tree
          (* We somehow managed to delete the dummy node at the base of the tree *)
          | None -> raise (FailedUpdate (update#key, update#id)) in
      let metadata = Doi_map.remove update#id index.metadata in
      {index with index_tree=index_tree; metadata=metadata} in
    (* Add the new version of the documents if the deleted flag is not set *)
    match (update#doc, update#value#deleted) with
      | (None, _) | (_,true) -> {index with last_update=update#key}
      | (Some json, false) ->
          let doc = document_of_json json in
          let index_tree =
            List.fold_left 
              (fun index_tree (eqnID,latex) -> Index_tree.add {doi=update#id; eqnID=eqnID; latex=latex} index_tree)
              index.index_tree
              doc#content in
          let metadata = Doi_map.add update#id (doc#journalID, doc#publicationYear, List.length doc#content) index.metadata in
          {last_update=update#key; index_tree=index_tree; metadata=metadata}
  with _ ->
    raise (FailedUpdate (update#key, update#id))

let rec run_update_batches index =
  let update_batch = get_update_batch index.last_update in
  let index = List.fold_left run_update index update_batch in
  save_index index;
  if List.length update_batch < batch_size then index else run_update_batches index

let run_updates () = 
  flush_line ("couchdb is at " ^ couchdb_url);
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

(* Introspection *)

let list_all () =
  flush_line ("couchdb is at " ^ couchdb_url);
  flush_line "Loading index";
  let index = load_index () in
  Doi_map.iter
    (fun doi (journalID, publicationYear, no_eqns) -> 
      flush_line ((decode_doi doi) ^ " journalID=" ^ journalID ^ " no_equations=" ^ (string_of_int no_eqns)))
    index.metadata;
  let no_eqns = Doi_map.fold (fun _ (_,_,no_eqns) total -> no_eqns+total) index.metadata 0 in
  flush_line ("Total number of equations: " ^ (string_of_int no_eqns))

let list_one doi =
  let doi = encode_doi doi in
  flush_line ("couchdb is at " ^ couchdb_url);
  flush_line "Loading index";
  let index = load_index () in
  flush_line ("Searching for " ^ doi);
  try
    let (journalID, publicationYear, no_eqns) = Doi_map.find (encode_doi doi) index.metadata in
    flush_line ((decode_doi doi) ^ " journalID=" ^ journalID ^ " no_equations=" ^ (string_of_int no_eqns))
  with Not_found ->
    flush_line "DOI not indexed"

let list_nodes () =
  flush_line ("couchdb is at " ^ couchdb_url);
  flush_line "Loading index";
  let index = load_index () in
  List.iter
    (fun eqn_node ->
      flush_line (string_of_int (Array.length eqn_node.latex)))
    (Index_tree.to_list index.index_tree)

(* Main *)

open Arg
let _ = parse
  [("-init", Unit init_index, ": Create an empty index")
  ;("-update", Unit run_updates, ": Update the index")
  ;("-query", Unit handle_queries, ": Handle index queries as a couchdb _external")
  ;("-list_all", Unit list_all, ": List all indexed keys")
  ;("-list", String list_one, ": List the entry for a given key")
  ;("-list_nodes", Unit list_nodes, ": Debugging tool: list the lengths of each equation node")]
  ignore
  "Use 'index -help' for available options"
