let main =
  let preprocess = Preprocessor.init () in
  let rec loop () =
    let input = read_line () in
    print_string ("Read: " ^ input); flush stdout;
    preprocess input;
    loop () in
  loop ()