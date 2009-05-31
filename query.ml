(* Compound boolean queries *)

open Genlex

(* The query type *)
type t =
  | Latex of Latex.t
  | And of t * t
  | Or of t * t
  | Not of t

(* Total number of atoms in the query *)
let rec length query =
  match query with
    | Latex latex -> Array.length latex
    | And (query1,query2) -> (length query1) + (length query2)
    | Or (query1,query2) -> (length query1) + (length query2)
    | Not query -> length query

open Str

(* Quick and dirty lexer, tokens are: "latexstring" ) ( AND OR NOT *)
let tokens = regexp "\"[^\"]*\"\|(\|)\|AND\|OR\|NOT"
let lex str = full_split tokens str

exception Parse_error

(* A simple recursive descent parser. Not the prettiest but yacc would be overkill *)
let parse_query preprocess tokens =
  let rec parse_atom tokens =
    match tokens with
      | Text _ :: rest -> parse_atom rest
      | Delim "NOT" :: rest ->
          let (query,rest) = parse_atom rest in
          parse_compound (Not query) rest
      | Delim "(" :: rest ->
          let (query,rest) = parse_atom rest in
          in match rest with
            | Delim ")" :: rest -> parse_compound query rest
      | Delim "AND" ::rest | Delim "OR" :: rest | Delim ")" :: rest -> raise Parse_error
      | Delim latex_string :: rest ->
          let latex = preprocess (String.sub latex_string 1 (String.length latex_string - 2)) in
          parse_compound (Latex latex) rest
      | _ -> raise Parse_error

  and parse_compound query1 tokens =
    match tokens with
      | Text _ :: rest -> parse_compound query1 rest
      | Delim "AND" :: rest ->
          let (query2,rest) = parse_atom rest in
          (And (query1,query2), rest)
      | Delim "OR" :: rest ->
          let (query2,rest) = parse_atom rest in
          (Or (query1,query2), rest)
      | Delim "(" :: rest | Delim ")" :: rest | Delim "NOT" :: rest -> raise Parse_error
      | [] -> (query1,[])
      | _ -> raise Parse_error in

 match parse_atom tokens with
  | (query,_) -> query

let of_string preprocess str = parse_query preprocess (lex str)

(* Extending the edit distance on latex strings to edit distance on compound queries *)

let rec distance query latex =
  match query with
    | Latex query_latex -> Edit.left_edit_distance query_latex latex
    | And (query1,query2) -> (distance query1 latex) + (distance query2 latex)
    | Or (query1,query2) -> min (distance query1 latex) (distance query2 latex)
    | Not query -> distance_not query latex

and distance_not query latex =
  match query with
    | Latex query_latex -> (Array.length query_latex) - (Edit.left_edit_distance query_latex latex)
    | And (query1,query2) -> min (distance_not query1 latex) (distance_not query2 latex)
    | Or (query1,query2) -> (distance_not query1 latex) + (distance_not query2 latex)
    | Not query -> distance query latex