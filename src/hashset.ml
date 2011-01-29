(* Simple sets using Hashtbl *)

type 'a t = ('a, unit) Hashtbl.t

let create = Hashtbl.create

let mem = Hashtbl.mem

let add ht key = Hashtbl.replace ht key ()

let to_list ht = Hashtbl.fold (fun k v rest -> k :: rest) ht []

let of_list list = 
  let ht = create 0 in 
  List.map (add ht) list;
  ht

let union ht1 ht2 = 
  let ht3 = create 0 in
  Hashtbl.iter (fun key _ -> add ht3 key) ht1;
  Hashtbl.iter (fun key _ -> add ht3 key) ht2;
  ht3

let inter ht1 ht2 =
  let ht3 = create 0 in
  Hashtbl.iter (fun key _ -> if mem ht2 key then add ht3 key else ()) ht1;
  ht3

let filter f ht =
  Hashtbl.iter
    (fun elem _ ->
      if f elem
      then () 
      else Hashtbl.remove ht elem) 
    ht
