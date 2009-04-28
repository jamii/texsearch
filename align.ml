(* Ew - fix the terrible variable naming *)
(* Will be a functor of metric *)

open Util
open Tree
open Metric

let rec align_tree cached t1 t2 =
  match (t1,t2) with
    | (Empty, Empty) -> 0.
    | (Empty, Branch (l2 ,cs2)) -> (metric None (Some l2)) +. cached [] cs2
    | (Branch (l1, cs1), Empty) -> (metric (Some l1) None) +. cached cs1 []
    | (Branch (l1, cs1), Branch (l2, cs2)) ->
        (* align t1 with t2 *)
        let cost = (metric (Some l1) (Some l2)) +. (cached cs1 cs2) in
        (* align t1 with some child of t2 *)
        let cost = if cs2 == [] then cost else min cost
          ((align_tree cached Empty t2) +.
            (minimum
              (List.map
                (fun c -> align_tree cached t1 c -. align_tree cached Empty c)
                cs2))) in
        (* align some child of t1 with t2 *)
        let cost = if cs1 == [] then cost else min cost
          ((align_tree cached t1 Empty) +.
            (minimum
              (List.map
                (fun c -> align_tree cached c t2 -. align_tree cached c Empty)
                cs1))) in
        cost

and align_forest cached f1 f2 =
  match (f1,f2) with
    | ([], []) -> 0.
    | ([], cs2) -> sum_float (List.map (align_tree cached Empty) cs2)
    | (cs1, []) -> sum_float (List.map (align_tree cached Empty) cs1)
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