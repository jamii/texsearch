open Util
open Tree

let metric a b =
  match (a,b) with
    | (None,Some _) -> 1.0
    | (Some _, None) -> 1.0
    | (Some a,Some b) ->
        if a = b then 0.0 else 2.0

let rec cost_of_forest forest =
  sum_float (List.map cost_of_tree forest)

and cost_of_tree tree =
  match tree with
    | Empty -> 0.0
    | Branch (label,forest) ->
        metric None (Some label) +. cost_of_forest forest