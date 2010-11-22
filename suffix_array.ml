type id = int
type pos = int

module Hashset = 
  struct
    type 'a t = ('a, unit) Hashtbl.t
    let create = Hashtbl.create
    let mem = Hashtbl.mem
    let add ht key = Hashtbl.replace ht key ()

    let to_list ht = Hashtbl.fold (fun k v rest -> k :: rest) ht []
    let of_list list = 
      let ht = create 0 in 
      List.map (add ht) list;
      ht

    let union ht1 ht2 = 
      let ht3 = create 0 in
      Hashtbl.iter (fun key _ -> add ht3 key) ht1;
      Hashtbl.iter (fun key _ -> add ht3 key) ht2;
      ht3

    let inter ht1 ht2 =
      let ht3 = create 0 in
      Hashtbl.iter (fun key _ -> if mem ht2 key then add ht3 key else ()) ht1;
      ht3
  end

type 'a t =
  { latexs : Latex.t DynArray.t 
  ; opaques : 'a DynArray.t
  ; mutable next_id : id
  ; mutable array : (id * pos) array
  ; mutable unsorted : ('a * Latex.t) list }

let create () =
  { latexs = DynArray.create ()
  ; opaques = DynArray.create ()
  ; next_id = 0
  ; array = Array.make 0 (0,0)
  ; unsorted = []}

let compare_suffix sa (id1,pos1) (id2,pos2) =
  let latex1, latex2 = DynArray.get sa.latexs id1, DynArray.get sa.latexs id2 in
  Latex.compare_suffix (latex1,pos1) (latex2,pos2)

let suffixes sa id =
  let latex = DynArray.get sa.latexs id in
  let n = Latex.length latex in
  List.map (fun pos -> (id,pos)) (Util.range 0 n)

let insert sa (opaque, latex) =
  let id = sa.next_id in
  sa.next_id <- id + 1;
  DynArray.add sa.opaques opaque;
  DynArray.add sa.latexs latex;
  id

let prepare sa =
  let ids = List.map (insert sa) sa.unsorted in
  let new_suffixes = Util.concat_map (suffixes sa) ids in
  let cmp = compare_suffix sa in
  let array = Array.of_list (List.merge cmp (List.fast_sort cmp new_suffixes) (Array.to_list sa.array)) in
  sa.unsorted <- [];
  sa.array <- array

let add sa latexs =
  sa.unsorted <- latexs @ sa.unsorted

let is_prefix sa latex1 (id,pos) =
  let latex2 = DynArray.get sa.latexs id in
  Latex.is_prefix (latex1,0) (latex2,pos)

let leq sa latex1 (id,pos) =
  let latex2 = DynArray.get sa.latexs id in
  (Latex.compare_suffix (latex1,0) (latex2,pos)) <= 0

(* binary search *)
let find_exact_into ids sa latex =
  (* find beginning of region *)
  (* lo < latex *)
  (* hi >= latex *)
  let rec narrow lo hi =
    let mid = lo + ((hi-lo) / 2) in
    if lo = mid then hi else
    if leq sa latex sa.array.(mid)
    then narrow lo mid
    else narrow mid hi in
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
  traverse (narrow (-1) (n-1))

let exact_match sa id =
  (0, DynArray.get sa.opaques id)

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

let gather_approx sa precision latex =
  let k = int_of_float (ceil ((1.0 -. precision) *. (float_of_int (Latex.length latex)))) in
  let ids = Hashset.create 0 in
  List.iter (find_exact_into ids sa) (Latex.fragments latex k);
  ids

let find_approx sa precision latex =
  let k = int_of_float (ceil ((1.0 -. precision) *. (float_of_int (Latex.length latex)))) in
  let ids = gather_approx sa precision latex in
  Util.filter_map (approx_match sa latex k) (Hashset.to_list ids)

(* replace by query_dist *)
let rec query_match sa query precision id =
  let rec qm query =
    match query with
    | Query.Latex (latex1,_) -> 
	let k = int_of_float (ceil ((1.0 -. precision) *. (float_of_int (Latex.length latex1)))) in
	let latex2 = DynArray.get sa.latexs id in
	let dist = Edit.left_edit_distance latex1 latex2 in
	if dist < k then Some dist else None
    | Query.And (query1, query2) -> 
	begin
	  match (qm query1, qm query2) with
	  | (Some dist1, Some dist2) -> Some (max dist1 dist2)
	  | _ -> None
	end
    | Query.Or (query1, query2) -> 
	begin
	  match (qm query1, qm query2) with
	  | (Some dist1, Some dist2) -> Some (min dist1 dist2)
	  | (Some dist1, None) -> Some dist1
	  | (None, Some dist2) -> Some dist2
	  | (None, None) -> None 
	end in
  match qm query with
  | Some dist ->
      let opaque = DynArray.get sa.opaques id in
      Some (dist, opaque)
  | None -> 
      None

let find_query sa precision query = 
  let rec fq query = (* gather_query *)
    match query with
    | Query.Latex (latex, _) -> gather_approx sa precision latex
    | Query.And (query1, query2) -> Hashset.inter (fq query1) (fq query2)
    | Query.Or (query1, query2) -> Hashset.union (fq query1) (fq query2) in
  let ids = fq query in
  Util.filter_map (query_match sa query precision) (Hashset.to_list ids)
