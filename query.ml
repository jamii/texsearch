(* Compound boolean queries *)

(* The query type *)
type t =
  | Latex of (Latex.t list) * string (* Store the string version so we can send the query back to the users *)
  | And of t * t
  | Or of t * t

(* Longest latex string in query *)
let rec max_length query =
  match query with
    | Latex (lines,_) -> Util.maximum (List.map Array.length lines)
    | And (query1,query2) -> max (max_length query1) (max_length query2)
    | Or (query1,query2) -> max (max_length query1) (max_length query2)

let is_blank_string str = 
  let blank = ref true in
  String.iter
    (fun char -> 
      if char <> ' ' 
      then blank := false
      else ())
    str;
  !blank

let is_quoted_string str =
  (String.get str 0 == '"') && (String.get str (String.length str - 1) == '"')

open Str

(* Quick and dirty lexer, delimiters are: "latexstring" ) ( AND OR *)
let token_spec = regexp "\"[^\"]*\"\|(\|)\|AND\|OR"
let lex str = 
  let tokens = full_split token_spec str in
  let tokens = 
    List.filter 
      (function
        | Text text when is_blank_string text -> false
        | _ -> true)
      tokens in
  Stream.of_list tokens

(* A simple recursive descent parser *)
let parse_query preprocesser tokens =
  let rec parse_atom = 
    parser
      | [< 'Delim "("; q=parse_expr; 'Delim ")" >] -> q
      | [< 'Delim delim when is_quoted_string delim >] -> 
          let text = String.sub delim 1 (String.length delim - 2) in
          let (lines, plain) = preprocesser text in
          Latex (lines, plain)

  and parse_expr = 
    parser
      | [< q1=parse_atom; stream >] ->
          (parser
            | [< 'Delim "AND"; q2=parse_expr >] -> And (q1, q2)
            | [< 'Delim "OR"; q2=parse_expr >] -> Or (q1, q2)
            | [< >] -> q1)
          stream

  and parse_query =
    parser
      | [< q=parse_expr; stream >] ->
          Stream.empty stream; q in

  parse_query tokens

exception Parse_error

let of_string preprocesser str = 
  try
    parse_query preprocesser (lex str)
  with _ ->
    raise Parse_error (* Dont care whether the error was parsing the query or preprocessing the latex *)

let rec to_string query =
  match query with
    | Latex (_,plain) -> "\"" ^ plain ^ "\""
    | And (query1,query2) -> "(" ^ (to_string query1) ^ " AND " ^ (to_string query2) ^ ")"
    | Or (query1,query2) -> "(" ^ (to_string query1) ^ " OR " ^ (to_string query2) ^ ")"

(* Extending the edit distance on latex strings to edit distance on compound queries *)
(* With the Edit.left_edit_distance as a quasi-metric this is a valid query for the bktree index *)
let rec query_dist query latex =
  match query with
    | Latex (lines,_) -> Util.minimum (List.map (fun line -> Edit.left_edit_distance line latex) lines)
    | And (query1,query2) -> max (query_dist query1 latex) (query_dist query2 latex)
    | Or (query1,query2) -> min (query_dist query1 latex) (query_dist query2 latex)
