(* 
Suffix arrays storing compressed latex formulae.
Allows neighbourhood search by Latex.distance
*)

open Util

type id = Suffix.id
type pos = Suffix.pos

type 'a t =
  { latexs : Latex.t DynArray.t 
  ; opaques : 'a DynArray.t
  ; deleted : bool DynArray.t
  ; mutable next_id : id
  ; mutable array : Suffix.t array
  ; mutable unsorted : ('a * Latex.t) list }

let create () =
  { latexs = DynArray.create ()
  ; opaques = DynArray.create ()
  ; deleted = DynArray.create ()
  ; next_id = 0
  ; array = [||]
  ; unsorted = []}

let ancientify sa =
  sa.array <- Ancient.follow (Ancient.mark sa.array);
  Gc.full_major ()

let add sa latexs =
  sa.unsorted <- latexs @ sa.unsorted

let compare_suffix sa (id1,pos1) (id2,pos2) =
  let latexL, latexR = DynArray.get sa.latexs id1, DynArray.get sa.latexs id2 in
  Latex.compare_suffix (latexL,pos1) (latexR,pos2)

let suffixes sa id =
  let latex = DynArray.get sa.latexs id in
  let n = Latex.length latex in
  List.map (fun pos -> Suffix.pack (id,pos)) (Util.range 0 n)

let insert sa (opaque, latex) =
  let id = sa.next_id in
  sa.next_id <- id + 1;
  DynArray.add sa.opaques opaque;
  DynArray.add sa.latexs latex;
  DynArray.add sa.deleted false;
  id

(* a little convoluted to keep memory usage as low as possible *)
let prepare sa =
  let ids = List.map (insert sa) sa.unsorted in
  sa.unsorted <- [];
  let new_suffixes = Util.concat_map (suffixes sa) ids in
  let old_len = Array.length sa.array in
  let new_len = List.length new_suffixes in
  let array = Array.make (old_len + new_len) (Suffix.pack (0,0)) in
  Array.blit sa.array 0 array 0 old_len;
  sa.array <- array;
  let index = ref old_len in
  List.iter 
    (fun suffix -> 
      array.(!index) <- suffix; 
      index := !index + 1)
    new_suffixes;
  let cmp suffix1 suffix2 = 
    let (id1,pos1) = Suffix.unpack suffix1 in
    let (id2,pos2) = Suffix.unpack suffix2 in
    compare_suffix sa (id1,pos1) (id2,pos2) in
  Array.fast_sort cmp sa.array

let delete sa filter =
  let deleted_ids = 
    Util.filter_map
      (fun id ->
	if filter (DynArray.get sa.opaques id)
        then Some id
        else None)
      (Util.range 0 (DynArray.length sa.opaques)) in
  List.iter (fun id -> DynArray.set sa.deleted id true) deleted_ids

let filter_deleted sa ids =
  Hashset.filter (fun id -> not (DynArray.get sa.deleted id)) ids

let is_prefix sa latexL (id,pos) =
  let latexR = DynArray.get sa.latexs id in
  Latex.is_prefix (latexL,0) (latexR,pos)

let leq sa latexL (id,pos) =
  let latexR = DynArray.get sa.latexs id in
  (Latex.compare_suffix (latexL,0) (latexR,pos)) <= 0

(* Exact searching *)

(* binary search *)
let gather_exact ids sa latex =
  (* find beginning of region *)
  (* lo < latex *)
  (* hi >= latex *)
  let rec narrow lo hi =
    let mid = lo + ((hi-lo) / 2) in
    if lo = mid then hi else
    if leq sa latex (Suffix.unpack sa.array.(mid))
    then narrow lo mid
    else narrow mid hi in
  let n = Array.length sa.array in
  let rec traverse index =
    if index >= n then () else
    let (id, pos) = Suffix.unpack sa.array.(index) in
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
  filter_deleted sa ids;
  List.map (exact_match sa) (Hashset.to_list ids)

(* Searching by Latex.distance *)

(*
The logic behind the approx search is as follows:
Suppose    Latex.distance latex corpus_term < k
Then       List.exists (fun fragment -> Latex.distance fragment corpus_term = 0) (Latex.fragments latex k) 
*)
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
  filter_deleted sa ids;
  Util.filter_map (approx_match sa precision latex) (Hashset.to_list ids)

(* Searching by Query.distance *)

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
  filter_deleted sa ids;
  Util.filter_map (query_match sa precision query) (Hashset.to_list ids)
