let random_array max_length gen =
  let length = (1 + Random.int max_length) in
  Array.map (fun _ -> gen ()) (Array.make length 0) 

let random_list max_length gen =
  Array.to_list (random_array max_length gen)

let random_latex_element () = 
  Random.int 50

let random_latex max_length =
  Latex.of_array (random_array max_length random_latex_element)

let test_search test search =
  let latex = random_latex 5 in
  let latexs = random_list 1000 (fun () -> random_latex 1000) in
  let ids = Util.range 0 (List.length latexs) in
  let items = List.combine ids latexs in
  Util.flush_line "Building...";
  let sa = Suffix_array.create () in
  Suffix_array.add sa items;
  Util.flush_line "Test...";
  let test_result = List.sort compare (test items latex) in
  Util.flush_line "Real...";
  let real_result = List.sort compare (search sa latex) in
  (test_result = real_result, test_result, real_result)

let test_exact_search () =
  let test items latex = 
    Util.filter_map 
      (fun (id,latex2) -> 
	if Edit.left_edit_distance latex latex2 = 0
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
      (fun (id,latex2) -> 
	if Edit.left_edit_distance latex latex2 < distance
	then Some id
	else None)
      items in
  let search sa latex =
    let results = Suffix_array.find_approx sa latex distance in
    List.map (fun (_,id) -> id) results in
  test_search test search
