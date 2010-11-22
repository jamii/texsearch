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

let test_find test find n =
  let latexs = random_list n (fun () -> random_latex 1000) in
  let opaques = random_list n (fun () -> random_string 1000) in
  let items = List.combine opaques latexs in
  let sa = Suffix_array.create () in
  Suffix_array.add sa items;
  Suffix_array.prepare sa;
  let test_result = List.sort compare (test items) in
  let real_result = List.sort compare (List.map (fun (_,opaque) -> opaque) (find sa)) in
  if test_result <> real_result then Util.flush_line "Fail!" else Util.flush_line "Pass!";
  (test_result = real_result, List.length test_result, List.length real_result)

let test_find_exact n =
  let latex = random_latex 5 in
  let test items = 
    Util.filter_map 
      (fun (id,latex2) -> 
	if Latex.distance latex latex2 = 0
	then Some id
	else None)
      items in
  let find sa =
    Suffix_array.find_exact sa latex in
  test_find test find n

let approx_match precision latex1 latex2 =
  let cutoff = int_of_float (ceil ((1.0 -. precision) *. (float_of_int (Latex.length latex1)))) in
  Latex.distance latex1 latex2 < cutoff

let test_find_approx n =
  let latex = random_latex 5 in
  let precision = Random.float 1.0 in
  let test items =
    Util.filter_map 
      (fun (id,latex2) ->
	if approx_match precision latex latex2
	then Some id
	else None)
      items in
  let find sa =
    Suffix_array.find_approx sa precision latex in
  test_find test find n

let rec query_match precision query latex2 =
  match query with
  | Query.Latex (latex1,_) -> approx_match precision latex1 latex2
  | Query.And (query1, query2) -> (query_match precision query1 latex2) && (query_match precision query2 latex2)
  | Query.Or (query1, query2) -> (query_match precision query1 latex2) || (query_match precision query2 latex2)

let test_find_query n =
  let query = random_query 5 in
  let precision = Random.float 1.0 in
  let test items =
    Util.filter_map 
      (fun (id,latex2) ->
	if query_match precision query latex2
	then Some id
	else None)
      items in
  let find sa =
    Suffix_array.find_query sa precision query in
  test_find test find n
  
