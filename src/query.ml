(* Compound boolean queries *)

(* The query type *)
type t =
  | Latex of Latex.t * string (* Store the string version so we can send the query back to the users *)
  | And of t * t
  | Or of t * t

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
          let (latex, plain) = preprocesser text in
          Latex (latex, plain)

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
let rec distance query latexR =
  match query with
    | Latex (latexL,_) -> Latex.distance latexL latexR
    | And (query1,query2) -> max (distance query1 latexR) (distance query2 latexR)
    | Or (query1,query2) -> min (distance query1 latexR) (distance query2 latexR)

let rec similar precision query latexR =
  match query with
  | Latex (latexL,_) -> 
      Latex.similar precision latexL latexR
  | And (query1, query2) -> 
      begin
	match (similar precision query1 latexR, similar precision query2 latexR) with
	| (Some dist1, Some dist2) -> Some (max dist1 dist2)
	| _ -> None
      end
  | Or (query1, query2) -> 
      begin
	match (similar precision query1 latexR, similar precision query2 latexR) with
	| (Some dist1, Some dist2) -> Some (min dist1 dist2)
	| (Some dist1, None) -> Some dist1
	| (None, Some dist2) -> Some dist2
	| (None, None) -> None 
      end
