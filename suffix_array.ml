open Util

type id = int
type pos = int

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

let add sa latexs =
  sa.unsorted <- latexs @ sa.unsorted

let compare_suffix sa (id1,pos1) (id2,pos2) =
  let latexL, latexR = DynArray.get sa.latexs id1, DynArray.get sa.latexs id2 in
  Latex.compare_suffix (latexL,pos1) (latexR,pos2)

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

let delete sa filter =
  let deleted_ids = 
    Util.filter_map
      (fun id ->
	if filter (DynArray.get sa.opaques id)
        then Some id
        else None)
      (Util.range 0 (DynArray.length sa.opaques)) in
  let retain (id, pos) = not (List.mem id deleted_ids) in
  sa.array <- Array.of_list (List.filter retain (Array.to_list sa.array))

let is_prefix sa latexL (id,pos) =
  let latexR = DynArray.get sa.latexs id in
  Latex.is_prefix (latexL,0) (latexR,pos)

let leq sa latexL (id,pos) =
  let latexR = DynArray.get sa.latexs id in
  (Latex.compare_suffix (latexL,0) (latexR,pos)) <= 0

(* binary search *)
let gather_exact ids sa latex =
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
  gather_exact ids sa latex;
  List.map (exact_match sa) (Hashset.to_list ids)

let gather_approx sa precision latex =
  let k = Latex.cutoff precision latex in
  let ids = Hashset.create 0 in
  List.iter (gather_exact ids sa) (Latex.fragments latex k);
  ids

let approx_match sa precision latexL id =
  let latexR = DynArray.get sa.latexs id in
  match Latex.similar precision latexL latexR with
  | Some dist ->
      let opaque = DynArray.get sa.opaques id in
      Some (dist, opaque) 
  | None ->
      None

let find_approx sa precision latex =
  let ids = gather_approx sa precision latex in
  Util.filter_map (approx_match sa precision latex) (Hashset.to_list ids)

let rec gather_query sa precision query =
  match query with
  | Query.Latex (latex, _) -> gather_approx sa precision latex
  | Query.And (query1, query2) -> Hashset.inter (gather_query sa precision query1) (gather_query sa precision query2)
  | Query.Or (query1, query2) -> Hashset.union (gather_query sa precision query1) (gather_query sa precision query2)

let query_match sa precision query id =
  let latexR = DynArray.get sa.latexs id in
  match Query.similar precision query latexR with
  | Some dist ->
      let opaque = DynArray.get sa.opaques id in
      Some (dist, opaque)
  | None -> 
      None

let find_query sa precision query = 
  let ids = gather_query sa precision query in
  Util.filter_map (query_match sa precision query) (Hashset.to_list ids)
