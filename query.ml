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

(* A simple recursive descent parser *)

let lex = make_lexer ["AND";"OR";"NOT";"(";")"]

let parse_query preprocess =
  let rec parse_query_atom = parser
    | [< 'String latex_string ; stream >] -> parse_query_compound (Latex (preprocess latex_string)) stream
    | [< 'Kwd "NOT" ; query = parse_query_atom ; stream >] -> parse_query_compound (Not query) stream
    | [< 'Kwd "(" ; query = parse_query_atom ; 'Kwd ")" ; stream >] -> parse_query_compound query stream

  and parse_query_compound query1 = parser
    | [< 'Kwd "AND" ; query2 = parse_query_atom >] -> And (query1,query2)
    | [< 'Kwd "OR" ; query2 = parse_query_atom >] -> Or (query1,query2)
    | [< >] -> query1 in
 parser
  | [< query = parse_query_atom ; stream >] -> Stream.empty stream; query

exception Parse_error

let of_string preprocess str =
  try
    parse_query preprocess (lex (Stream.of_string str))
  with Stream.Error _ | Stream.Failure | Parsing.Parse_error | Json_type.Json_error _ | Latex.Parse_error ->
    raise Parse_error

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