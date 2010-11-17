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

let test_search test search n =
  let latex = random_latex 5 in
  let latexs = random_list n (fun () -> random_latex 1000) in
  let ids = random_list n (fun () -> random_string 1000) in
  let items = List.combine ids latexs in
  let sa = Suffix_array.create () in
  Suffix_array.add sa items;
  Suffix_array.prepare sa;
  let test_result = List.sort compare (test items latex) in
  let real_result = List.sort compare (search sa latex) in
  if test_result <> real_result then Util.flush_line "Fail!" else ();
  (test_result = real_result, List.length test_result, List.length real_result)

let test_exact_search n =
  let test items latex = 
    Util.filter_map 
      (fun (id,latex2) -> 
	if Edit.left_edit_distance latex latex2 = 0
	then Some id
	else None)
      items in
  let search sa latex =
    Suffix_array.find_exact sa latex in
  test_search test search n

let test_approx_search n =
  let distance = Random.int 5 in
  let test items latex =
    Util.filter_map 
      (fun (id,latex2) -> 
	if Edit.left_edit_distance latex latex2 < distance
	then Some id
	else None)
      items in
  let search sa latex =
    let results = Suffix_array.find_approx sa latex distance in
    List.map (fun (_,id) -> id) results in
  test_search test search n
