exception Bad_request

let index = ref Mtree.empty

let run_query fragment limit cutoff =
  match Mtree.next limit cutoff (Mtree.search fragment !index) with
    | Mtree.Last results -> results
    | Mtree.More (results,_) -> results

let handle_get args =
  let fragment, limit, cutoff =
    try
        Latex.fragment_of_json (List.assoc "latex" args) ,
        (match (List.assoc "limit" args) with
          | Json_type.String limit -> int_of_string limit) ,
        (match (List.assoc "cutoff" args) with
          | Json_type.String cutoff -> float_of_string cutoff)
    with
      | Not_found | Failure _ | Latex.Bad_latex -> raise Bad_request in
  Json_type.Array
    (List.map
      (fun (id,rank) -> Json_type.Array [Json_type.String id ; Json_type.Float rank])
      (run_query fragment limit cutoff))

let handle_put args =
  let fragment, id =
    try
      Latex.fragment_of_json (List.assoc "latex" args) ,
      match (List.assoc "id" args) with
        | Json_type.String id -> id
    with
      | Not_found | Latex.Bad_latex -> raise Bad_request in
  index := Mtree.add (Mtree.node id fragment) !index;
  Json_type.Null

let handle_request request =
    try
      match Json_io.json_of_string ~recursive:true request with
        | Json_type.Object fields ->
            (* args *)
            let args = match List.assoc "query" fields with
              | Json_type.Object args -> args in
            (* verb *)
            match List.assoc "verb" fields with
              | Json_type.String "PUT" -> handle_put args
              | Json_type.String "GET" -> handle_get args
    with Json_type.Json_error _ | Match_failure _ | Not_found -> raise Bad_request

let handle_requests () =
  while true do
    let request = read_line () in
    let response, code =
      try
        handle_request request, Json_type.Int 200 (* OK *)
      with
        | Bad_request -> Json_type.Null, Json_type.Int 400 (* Bad request *)
        | _ -> Json_type.Null, Json_type.Int 500 (* Internal server error *) in
    let output =
      Json_io.string_of_json ~compact:true
        (Json_type.Object
          [ ("code",code)
          ; ("response",response) ]) in
    print_string output; print_string "\n"; flush stdout
  done