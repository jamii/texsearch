let flush_line str = print_string str; print_string "\n"; flush stdout

let minimum (l::ls) = List.fold_left min l ls

let maximum (l::ls) = List.fold_left max l ls

let filter_map f ls =
  List.map 
    (fun l -> match l with Some a -> a) 
    ((List.filter 
      (fun l -> l <> None) 
      (List.map f ls)))

let concat_map f ls =
  List.fold_right (@) (List.map f ls) []

let rec range start finish = 
  if start < finish then start :: range (start+1) finish else []

(* Fairly hackish method of sucking out stream elements *)
let list_of_stream stream = Stream.npeek max_int stream 

let load_data filename =
  try
    let data_file = open_in_bin filename in
    let data = Marshal.from_channel data_file in
    close_in data_file; data
  with _ ->
    flush_line ("Error opening file " ^ filename);
    raise Exit

let save_data filename data =
  try
    let data_file = open_out_bin (filename ^ "_tmp") in
    Marshal.to_channel data_file data [];
    close_out data_file;
    Unix.rename (filename ^ "_tmp") filename
  with _ ->
    flush_line ("Error saving to file " ^ filename);
    raise Exit

(* Tune the gc for lots of garbage *)
open Gc
let expect_garbage () =
  let m = 1024 * 1024 in
  Gc.set 
    {(Gc.get ()) with
      minor_heap_size = 256 * m;
      major_heap_increment = 64 * m;
      space_overhead = 200
    }

let backtrace f =
  Printexc.record_backtrace true;
  try Printexc.print f (); () with _ -> Printexc.print_backtrace stdout
