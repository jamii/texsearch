type id = int
type pos = int

type t = int
      
let pack_size = (Sys.word_size / 2) - 1
let max_size = 1 lsl pack_size
    
exception Invalid_suffix of id * pos
      
let pack (id, pos) = 
  if (id < 0) || (id >= max_size)
  || (pos < 0) || (pos >= max_size)
  then raise (Invalid_suffix (id, pos))
  else pos lor (id lsl pack_size)
      
let unpack suffix =
  let id = suffix lsr pack_size in
  let pos = suffix land (max_size - 1) in
  (id, pos)
