let flush_line str = print_string str; print_string "\n"; flush stdout

let lock () =
  try
    flush_line "Checking pid file";
    let pid_file = open_in "run/update.pid" in
    let pid = try input_line pid_file with End_of_file -> "" in
    begin
      match (pid, Unix.system ("ps " ^ pid ^ " &> /dev/null")) with
      | ("", _) ->
	  flush_line "No existing pid"
      | (pid, Unix.WEXITED 0) ->
	  flush_line ("Process with pid " ^ pid ^ " already exists");
	  raise Exit
      | (pid, Unix.WEXITED 1) ->
	  flush_line ("Process with pid " ^ pid ^ " does not exist")
    end;
    close_in pid_file;
    let pid_file = open_out "run/update.pid" in
    output_string pid_file (string_of_int (Unix.getpid ()));
    close_out pid_file
  with 
  | Exit ->
      raise Exit
  | exc ->
      flush_line "Error checking pid in run/update.pid";
      raise exc
