let init () =
  let pin, pout = Unix.open_process "python preprocessor.py" in
  fun str ->
    output_string pout (str ^ "\n");
    flush pout;
    input_line pin