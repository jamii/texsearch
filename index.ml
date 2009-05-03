exception Bad_request

let index = ref Mtree.empty

(* Query handler *)

type json query_args =
  < query :
    < latex : string
    ; limit : string
    ; cutoff : string > >

type json query_results = (string * float) list

let run_query fragment limit cutoff =
  match Mtree.next limit cutoff (Mtree.search fragment !index) with
    | Mtree.Last results -> results
    | Mtree.More (results,_) -> results

let handle_query str =
  let response, code =
    try
      let query = (query_args_of_json (Json_io.json_of_string str))#query in
      let fragment = Latex.fragment_of_json (Json_io.json_of_string query#latex) in
      let limit = int_of_string query#limit in
      let cutoff = float_of_string query#cutoff in
      json_of_query_results (run_query fragment limit cutoff), Json_type.Int 200 (* OK *)
    with
      | Json_type.Json_error _ | Latex.Bad_latex | Failure _ -> Json_type.Null, Json_type.Int 400 (* Bad request *)
      | _ -> Json_type.Null, Json_type.Int 500 (* Internal server error *) in
  let output =
    Json_io.string_of_json ~compact:true
      (Json_type.Object
        [ ("code",code)
        ; ("json",response) ]) in
  print_string (output ^ "\n"); flush stdout

(* Update handlers *)

type json update =
  < typ "type" : string
  ; id : string
  ; body "new" : Json_type.t >

type json document =
  < content : Json_type.t >

let handle_update str =
  try
    let update = (update_of_json (Json_io.json_of_string str)) in
    match update#typ with
      | "create" ->
          let doc = document_of_json update#body in
          let fragment = Latex.fragment_of_json doc#content in
          index := Mtree.add (Mtree.node update#id fragment) !index
      | "delete" ->
          let doc = document_of_json update#body in
          index := Mtree.delete update#id !index
      | "update" ->
          print_string "update"; print_newline (); flush stdout;
          let doc = document_of_json update#body in
          let fragment = Latex.fragment_of_json doc#content in
          index := Mtree.add (Mtree.node update#id fragment) (Mtree.delete update#id !index)
  with _ -> print_string "Aaaaaaaahhhhhh"; print_newline (); flush stdout (* Error handling? *)

let run_handlers () =
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