open Tree
open Util

type t = int list

let make n = Array.init n (fun k -> 0)

let path_lengths n forest =
  let hist = make n in
  let rec fill_hist tree =
    match tree with
      | Empty -> 0
      | Branch (_,[]) ->
          hist.(0) <- hist.(0) + 1; 0
      | Branch (_,cs) ->
          let path_length = (1 + maximum (List.map fill_hist cs)) mod n in
          hist.(path_length) <- hist.(path_length) + 1; path_length in
  ignore (List.map fill_hist forest);
  Array.to_list hist

let l1_norm hist1 hist2 =
  List.fold_left2 (fun total val1 val2 -> total + (abs (val1 - val2))) 0 hist1 hist2

(* lower_bound n costs hist1 hist2 *)
