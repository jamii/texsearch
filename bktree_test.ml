let rand_int () = Random.int 100 

let rec rand_int_list n =
  if n = 0 then [] else (rand_int ()) :: (rand_int_list (n-1))

(* Euclidean dist and query *)

module M1 : (
sig 
  type t = int
  val dist : int -> int -> int
end) =
struct
  type t = int
  let dist a b = abs (a - b)
end

module Int_tree1 = Bktree.Make (M1)

let search_test1 no_nodes seed =
  Random.init seed;
  let cutoff = rand_int () in
  let target = rand_int () in 
  let query = M1.dist target in
  let nodes = rand_int_list no_nodes in
  let tree = Int_tree1.of_list nodes in
  let results = List.sort compare (Stream.npeek no_nodes (Int_tree1.run_query query cutoff tree)) in
  let correct_results = List.sort compare (List.filter (fun (_,d) -> d < cutoff) (List.map (fun node -> (node, query node)) nodes)) in
  (results = correct_results, target, cutoff, results, correct_results, nodes)

(* asymmetric dist and decreasing query *)

module M2 : (
sig 
  type t = int
  val dist : int -> int -> int
end) =
struct
  type t = int
  let dist a b = if a > b then 100 else 0
end

module Int_tree2 = Bktree.Make (M2)

let search_test2 no_nodes seed = 
  Random.init seed;
  let cutoff = rand_int () in
  let target = rand_int () in 
  let query b = 100 / (b+1) in
  let nodes = rand_int_list no_nodes in
  let tree = Int_tree2.of_list nodes in
  let results = List.sort compare (Stream.npeek no_nodes (Int_tree2.run_query query cutoff tree)) in
  let correct_results = List.sort compare (List.filter (fun (_,d) -> d < cutoff) (List.map (fun node -> (node, query node)) nodes)) in
  (results = correct_results, target, cutoff, results, correct_results, nodes)

let filter_test no_nodes seed =
  Random.init seed;
  let limit = rand_int () in
  let filter b = b < limit in
  let nodes = rand_int_list no_nodes in
  let results = 
    List.sort compare (Int_tree2.to_list
      (match Int_tree2.filter filter (Int_tree2.of_list nodes) with Some tree -> tree)) in
  let correct_results = List.sort compare (List.filter filter nodes) in
  (results = correct_results, limit, results, correct_results, nodes)


