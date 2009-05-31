open Genlex

type t =
  | Latex of Latex.t
  | And of t * t
  | Or of t * t
  | Not of t

let lex = make_lexer ["AND";"OR";"NOT";"(";")"]

let rec parse_query_atom = parser
  | [< 'String latex_string ; stream >] ->
      let latex = Latex.of_json (Json_io.json_of_string latex_string) in
      parse_query_compound (Latex latex) stream
  | [< 'Kwd "NOT" ; query = parse_query_atom ; stream >] -> parse_query_compound (Not query) stream
  | [< 'Kwd "(" ; query = parse_query_atom ; 'Kwd ")" ; stream >] -> parse_query_compound query stream

and parse_query_compound query1 = parser
  | [< 'Kwd "AND" ; query2 = parse_query_atom >] -> And (query1,query2)
  | [< 'Kwd "OR" ; query2 = parse_query_atom >] -> Or (query1,query2)
  | [< >] -> query1

let parse_query = parser
  | [< query = parse_query_atom ; stream >] -> Stream.empty stream; query

exception Parse_error

let of_string str =
  try
    parse_query (lex (Stream.of_string str))
  with Stream.Error _ | Stream.Failure | Parsing.Parse_error | Json_type.Json_error _ | Latex.Parse_error ->
    raise Parse_error

let rec length query =
  match query with
    | Latex latex -> Array.length latex
    | And (query1,query2) -> (length query1) + (length query2)
    | Or (query1,query2) -> (length query1) + (length query2)
    | Not query -> length query

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