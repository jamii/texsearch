let random_array max_length gen =
  let length = Random.int max_length in
  Array.map (fun _ -> gen ()) (Array.make length 0) 

let random_list max_length gen =
  Array.to_list (random_array max_length gen)

let random_latex_element () = 
  Random.int max_int

let random_latex () =
  Latex.of_array (random_array (1000) random_latex_element)

let test_search test search =
  let latex = random_latex () in
  let latexs = random_list (10000) random_latex in
  let ids = Util.range 0 (List.length latexs) in
  let items = List.combine ids latexs in
  Util.flush_line "Building...";
  let sa = Suffix_array.create () in
  Suffix_array.add sa items;
  Util.flush_line "Test...";
  let test_result = test items latex in
  Util.flush_line "Real...";
  let real_result = search sa latex in
  (test_result == real_result, test_result, real_result)

let test_exact_search () =
  let test items latex = 
    Util.filter_map 
      (fun (id,latex') -> 
	if latex = latex'
	then Some id
	else None)
      items in
  let search sa latex =
    Suffix_array.find_exact sa latex in
  test_search test search

let test_approx_search () =
  let distance = Random.int 5 in
  let test items latex =
    Util.filter_map 
      (fun (id,latex') -> 
	if Edit.left_edit_distance latex latex' < distance
	then Some id
	else None)
      items in
  let search sa latex =
    let results = Suffix_array.find_approx sa latex distance in
    List.map (fun (_,id) -> id) results in
  test_search test search
