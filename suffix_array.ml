type id = int
type pos = int

module Hashset = 
  struct
    type 'a t = ('a, unit) Hashtbl.t
    let create = Hashtbl.create
    let mem = Hashtbl.mem
    let add ht key = Hashtbl.add ht key ()
    let to_list ht = Hashtbl.fold (fun k v rest -> k :: rest) ht []
  end

type 'a t =
  { latexs : Latex.t DynArray.t 
  ; opaques : 'a DynArray.t
  ; mutable next_id : id
  ; mutable array : (id * pos) array }

let create () =
  { latexs = DynArray.create ()
  ; opaques = DynArray.create ()
  ; next_id = 0
  ; array = Array.make 0 (0,0) }

let compare_suffix sa (id1,pos1) (id2,pos2) =
  let latex1, latex2 = DynArray.get sa.latexs id1, DynArray.get sa.latexs id2 in
  Latex.compare_suffix (latex1,pos1) (latex2,pos2)

let suffixes sa id =
  let latex = DynArray.get sa.latexs id in
  let n = Latex.length latex in
  List.map (fun pos -> (id,pos)) (Util.range 0 n)

let add_latex sa (opaque, latex) =
  let id = sa.next_id in
  sa.next_id <- id + 1;
  DynArray.set sa.opaques id opaque;
  DynArray.set sa.latexs id latex;
  id

let add sa latexs =
  let ids = List.map (add_latex sa) latexs in
  let new_suffixes = Util.concat_map (suffixes sa) ids in
  let cmp = compare_suffix sa in
  let array = Array.of_list (List.merge cmp (List.fast_sort cmp new_suffixes) (Array.to_list sa.array)) in
  sa.array <- array

let is_prefix sa latex1 (id,pos) =
  let latex2 = DynArray.get sa.latexs id in
  Latex.is_prefix (latex1,0) (latex2,pos)

let less_than sa latex1 (id,pos) =
  let latex2 = DynArray.get sa.latexs id in
  (Latex.compare_suffix (latex1,0) (latex2,pos)) < 0

(* binary search *)
let find_exact_into ids sa latex =
  (* find beginning of region *)
  let rec narrow lo hi =
    if lo = hi then lo else
    let mid = lo + ((hi-lo) / 2) in
    if less_than sa latex sa.array.(mid)
    then narrow lo mid
    else narrow mid hi in
  let ids = Hashset.create 0 in
  let n = Array.length sa.array in
  let rec traverse index =
    if index >= n then () else
    let (id, pos) = sa.array.(index) in
    if is_prefix sa latex (id, pos)
    then
      begin
	Hashset.add ids id; 
	traverse (index+1)
      end
    else () in
  traverse (narrow 0 (n-1)) (* !!! think about edge conditions of narrow *)

let exact_match sa id =
  DynArray.get sa.opaques id

let find_exact sa latex =
  let ids = Hashset.create 0 in
  find_exact_into ids sa latex;
  List.map (exact_match sa) (Hashset.to_list ids)

let approx_match sa latex1 k id =
  let latex2 = DynArray.get sa.latexs id in
  let dist = Edit.left_edit_distance latex1 latex2 in
  if dist < k 
  then 
    let opaque = DynArray.get sa.opaques id in
    Some (dist, opaque) 
  else 
    None

let find_approx sa latex k =
  let ids = Hashset.create 0 in
  List.iter (find_exact_into ids sa) (Latex.fragments latex k);
  Util.filter_map (approx_match sa latex k) (Hashset.to_list ids)
