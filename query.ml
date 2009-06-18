(* Compound boolean queries *)

open Genlex

(* The query type *)
type t =
  | Latex of Latex.t * string (* Store the string version so we can send the query back to the users *)
  | And of t * t
  | Or of t * t
  | Not of t

(* Longest latex string in query *)
let rec max_length query =
  match query with
    | Latex (latex,_) -> Array.length latex
    | And (query1,query2) -> max (max_length query1) (max_length query2)
    | Or (query1,query2) -> max (max_length query1) (max_length query2)
    | Not query -> max_length query

open Str

(* Quick and dirty lexer, tokens are: "latexstring" ) ( AND OR NOT *)
let tokens = regexp "\"[^\"]*\"\|(\|)\|AND\|OR\|NOT"
let lex str =
  List.filter
    (fun token -> match token with
      | Text _ -> false
      | Delim _ -> true)
  (full_split tokens str)

exception Parse_error

(* A simple recursive descent parser. Not the prettiest but yacc would be overkill *)
let parse_query preprocesser tokens =
  let rec parse_atom tokens =
    match tokens with
      | Delim "NOT" :: rest ->
          let (query,rest) = parse_atom rest in
          parse_compound (Not query) rest
      | Delim "(" :: rest ->
          let (query,rest) = parse_atom rest in
          (match rest with
            | Delim ")" :: rest -> parse_compound query rest
            | _ -> raise Parse_error)
      | Delim "AND" :: rest | Delim "OR" :: rest | Delim ")" :: rest -> raise Parse_error
      | Delim latex_string :: rest ->
          let (latex,plain) = preprocesser (String.sub latex_string 1 (String.length latex_string - 2)) in
          parse_compound (Latex (latex,plain)) rest
      | _ -> raise Parse_error

  and parse_compound query1 tokens =
    match tokens with
      | Delim "AND" :: rest ->
          let (query2,rest) = parse_atom rest in
          (And (query1,query2), rest)
      | Delim "OR" :: rest ->
          let (query2,rest) = parse_atom rest in
          (Or (query1,query2), rest)
      | Delim "(" :: rest | Delim "NOT" :: rest -> raise Parse_error
      | Delim ")" :: rest -> (query1, tokens)
      | [] -> (query1,[])
      | _ -> raise Parse_error in

 match parse_atom tokens with
  | (query,_) -> query

let of_string preprocesser str = parse_query preprocesser (lex str)

let rec to_string query =
  match query with
    | Latex (_,plain) -> "\"" ^ plain ^ "\""
    | And (query1,query2) -> "(" ^ (to_string query1) ^ " AND " ^ (to_string query2) ^ ")"
    | Or (query1,query2) -> "(" ^ (to_string query1) ^ " OR " ^ (to_string query2) ^ ")"
    | Not query -> "(NOT " ^ (to_string query) ^ ")"
