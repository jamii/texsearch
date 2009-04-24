(* Move to tree *)
(* Invariant - children are never Empty *)
type 'e tree =
  | Empty
  | Branch of 'e * 'e forest
and 'e forest = 'e tree list

type 'e metric = 'e option -> 'e option -> float

let sum ls = List.fold_left (+.) 0. ls

let minimum ls = List.fold_left min infinity ls

(* Pull dlist out to another module *)

type 'e dlist = 'e list -> 'e list

let empty = fun es -> es

let cons e dlist =
  fun es -> dlist (e :: es)

let to_list dlist = dlist []

let partitions rs =
  let rec loop ls rs =
    (to_list ls, rs) ::
    match rs with
      | [] -> []
      | r::rs -> loop (cons r ls) rs in
  loop empty rs

(* Ew - fix the terrible variable naming *)
(* Will be a functor of metric *)

let metric a b =
  if a = b
  then 0.
  else 1.

let rec align_tree cached t1 t2 =
  match (t1,t2) with
    | (Empty, Empty) -> 0.0
    | (Empty, Branch (l2 ,cs2)) -> (metric None (Some l2)) +. cached [] cs2
    | (Branch (l1, cs1), Empty) -> (metric (Some l1) None) +. cached cs1 []
    | (Branch (l1, cs1), Branch (l2, cs2)) ->
        minimum [
          (* align t1 with t2 *)
          (metric (Some l1) (Some l2)) +. (cached cs1 cs2) ;
          (* align t1 with some child of t2 *)
          (align_tree cached Empty t2) +.
          (minimum
            (List.map
              (fun c -> align_tree cached t1 c -. align_tree cached Empty c)
              cs2)) ;
          (* align some child of t1 with t2 *)
          (align_tree cached t1 Empty) +.
          (minimum
            (List.map
              (fun c -> align_tree cached c t2 -. align_tree cached c Empty)
              cs1)) ]

and align_forest cached f1 f2 =
  match (f1,f2) with
    | ([], []) -> 0.0
    | ([], cs2) -> sum (List.map (align_tree cached Empty) cs2)
    | (cs1, []) -> sum (List.map (align_tree cached Empty) cs1)
    | (c1::cs1, c2::cs2) ->
        minimum [
          (* align c1 with c2 *)
          (align_tree cached c1 c2) +. (cached cs1 cs2) ;
          (* align the root of c1 with nothing *)
          (* align the children of c1 with some part of f2 *)
          (* align cs1 with the rest of f2 *)
          (match c1 with
            | Branch (l1, cs3) ->
                (metric (Some l1) None) +.
                minimum
                  (List.map
                    (fun (csL, csR) -> (cached cs3 csL) +. (cached cs1 csR))
                    (partitions f2))) ;
          (* align the root of c2 with nothing *)
          (* align some part of f1 with the children of c2 *)
          (* align the rest of f1 with cs2 *)
          (match c2 with
            | Branch (l2, cs3) ->
                (metric None (Some l2)) +.
                minimum
                  (List.map
                    (fun (csL, csR) -> (cached csL cs3) +. (cached csR cs2))
                    (partitions f1))) ]

(* Move to align_test *)
let simple_metric a b =
  match (a,b) with
    | (None, Some b) -> abs_float b
    | (Some a, None) -> abs_float a
    | (Some a, Some b) -> abs_float (a -. b)

let tree1 =
  Branch (1.,[
    Branch (2.,[]) ;
    Branch (3.,[]) ;
    Branch (4.,[]) ])

let tree2 =
  Branch (1.,[
    Branch (8.,[]) ;
    Branch (3.,[]) ;
    Branch (5.,[]) ])

(* Move to ... *)
type element =
  | Tag of string * ((string*string) list)
  | Text of string

(* Assumes input is non-empty, will want to wrap this in try *)
let rec tree_of_xml xml =
  match xml with
    | Xml.Element (tag,attrs,xmls) -> Branch (Tag (tag,attrs), forest_of_xml xmls)
    | Xml.PCData text -> Branch (Text text, [])

and forest_of_xml xmls = List.map tree_of_xml xmls

let forest_of_document doc =
  match doc with
    | Xml.Element ("document",_,contents) -> forest_of_xml contents

let make_index str =
  try
    (let xml = Xml.parse_string str in
    match xml with
      | Xml.Element ("results",_,results) ->
          List.map forest_of_document results)
  with Xml.Error err ->
    print_string (Xml.error err); print_string "\n";[]