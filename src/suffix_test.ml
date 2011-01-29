let test_pack n =
  for i = 0 to n do
    let (id, pos) = (Random.int Suffix.max_size, Random.int Suffix.max_size) in
    if (id, pos) = Suffix.unpack (Suffix.pack (id,pos))
    then () (* Util.flush_line "Pass!" *)
    else Util.flush_line ("Fail!: " ^ (string_of_int id) ^ " " ^ (string_of_int pos))
  done
