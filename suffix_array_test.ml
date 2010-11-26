let random_array length gen =
  Array.map (fun _ -> gen ()) (Array.make length 0) 

let random_list length gen =
  Array.to_list (random_array length gen)

let random_latex_element () = 
  Random.int 50

let random_latex max_length =
  let length = (1 + Random.int max_length) in
  Latex.of_array (random_array length random_latex_element)

let random_string max_length =
  let length = (1 + Random.int max_length) in
  String.create length (* unitialised memory is fine *)

let rec random_query max_length =
  match Random.int 6 with
  | 0 -> Query.And (random_query max_length, random_query max_length) 
  | 1 -> Query.Or (random_query max_length, random_query max_length) 
  | _ -> Query.Latex (random_latex max_length, "")

let random_corpus n =
  let latexs = random_list n (fun () -> random_latex 1000) in
  let opaques = random_list n (fun () -> random_string 1000) in
  let items = List.combine opaques latexs in
  let sa = Suffix_array.create () in
  Suffix_array.add sa items;
  Suffix_array.prepare sa;
  let ((opaque,latex)::items) = items in
  Suffix_array.delete sa ((=) opaque);
  (items, sa)

let test_find test find n =
  let (items, sa) = random_corpus n in
  let test_result = List.sort compare (test items) in
  let real_result = List.sort compare (List.map (fun (_,opaque) -> opaque) (find sa)) in
  if test_result <> real_result then Util.flush_line "Fail!" else Util.flush_line "Pass!";
  (test_result = real_result, List.length test_result, List.length real_result)

let exact_match latexL latexR = 
  Latex.distance latexL latexR = 0

let test_find_exact n =
  let latexL = random_latex 5 in
  let test items = 
    Util.filter_map 
      (fun (id,latexR) -> 
	if exact_match latexL latexR
	then Some id
	else None)
      items in
  let find sa =
    Suffix_array.find_exact sa latexL in
  test_find test find n

let approx_match precision latexL latexR =
  Latex.similar precision latexL latexR <> None

let test_find_approx n =
  let latexL = random_latex 5 in
  let precision = Random.float 1.0 in
  let test items =
    Util.filter_map 
      (fun (id,latexR) ->
	if approx_match precision latexL latexR
	then Some id
	else None)
      items in
  let find sa =
    Suffix_array.find_approx sa precision latexL in
  test_find test find n

let rec query_match precision query latexR =
  Query.similar precision query latexR <> None

let test_find_query n =
  let query = random_query 5 in
  let precision = Random.float 1.0 in
  let test items =
    Util.filter_map 
      (fun (id,latexR) ->
	if query_match precision query latexR
	then Some id
	else None)
      items in
  let find sa =
    Suffix_array.find_query sa precision query in
  test_find test find n

let test_find_max_precision n =
  let latexL = random_latex 5 in
  let test items = 
    Util.filter_map 
      (fun (id,latexR) -> 
	if exact_match latexL latexR
	then Some id
	else None)
      items in
  let find sa =
    Suffix_array.find_approx sa 1.0 latexL in
  test_find test find n
