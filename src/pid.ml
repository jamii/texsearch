(* Prevents multiple update processes from running in parrallel *)

let lock () =
  try
    Util.flush_line "Checking pid file";
    let pid_file = open_in "run/update.pid" in
    let pid = try input_line pid_file with End_of_file -> "" in
    begin
      match (pid, Unix.system ("ps " ^ pid ^ " &> /dev/null")) with
      | ("", _) ->
	  Util.flush_line "No existing pid"
      | (pid, Unix.WEXITED 0) ->
	  Util.flush_line ("Process with pid " ^ pid ^ " already exists");
	  raise Exit
      | (pid, Unix.WEXITED 1) ->
	  Util.flush_line ("Process with pid " ^ pid ^ " does not exist")
    end;
    close_in pid_file;
    let pid_file = open_out "run/update.pid" in
    output_string pid_file (string_of_int (Unix.getpid ()));
    close_out pid_file
  with 
  | Exit ->
      raise Exit
  | exc ->
      Util.flush_line "Error checking pid in run/update.pid";
      raise exc
