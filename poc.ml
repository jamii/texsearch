open Align_opt

let discrete_metric a b =
  if a = b
  then 0.
  else 1.

let rec size tree =
  match tree with
    | Empty -> 0.
    | Branch (_,children) -> 1. +. (sum (List.map size children))

let print_element element =
  match element with
    | Tag (tag,attrs) ->
        print_string tag;
        List.iter (fun (a,b) -> print_string (" " ^ a ^ "=" ^ b)) attrs
    | Text text -> print_string text

let print_element_tree tree =
  let rec loop space tree =
    match tree with
      | Branch (l,cs) ->
          print_string space; print_element l; print_string "\n";
          List.iter (loop ("  "^space)) cs in
  loop "" tree

let print_element_forest forest =
  List.iter print_element_tree forest

let align = Cache.with_cache align_forest

let run_query query index =
  List.fast_sort
    (fun (rank1,_) (rank2,_) -> compare rank1 rank2)
    (List.map (fun t -> print_string "."; flush stdout;((align query t) (*/. (size t)*), t)) index)

let main =
  let preprocess = Preprocessor.init () in
  print_string "Welcome to texsearch\n";
  print_string "Enter a LaTeX string (all on one line)\n";
  print_string "eg. $$\\dot{V}$$\n";
  let loop () =
    print_string ("> ");
    let input = read_line () in
    let prep = preprocess input in
    try
      let query = forest_of_document (Xml.parse_string prep) in
      print_string "Your query parses as:\n";
      print_element_forest query;
      print_string ("Searching");
      flush stdout;
      let results = run_query query Index.index in
      print_string "The top 3 results are:\n";
      (match results with
        | a::b::c::_ ->
            let print_result (rank,tree) =
              print_string "Rank: "; print_float rank; print_string "\n";
              print_element_forest tree;
              print_string "\n" in
            print_result a;
            print_result b;
            print_result c);
    with Xml.Error err ->
      print_string (Xml.error err); print_string "\n";
      print_string prep in
  while true do loop () done

(* let _ = List.iter (fun forest -> print_element_forest forest; print_string "\n\n") Index.index *)