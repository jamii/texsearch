type 'e t = 'e list -> 'e list

let empty = fun es -> es

let snoc e dlist =
  fun es -> dlist (e :: es)

let to_list dlist = dlist []

let (@) dlist1 dlist2 = fun es -> dlist1 (dlist2 es)