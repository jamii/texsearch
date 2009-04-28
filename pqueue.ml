type ('e,'p) t = ('e * 'p) list

let by_priority (_,p1) (_,p2) = compare p1 p2

let empty = []

let add key priority pqueue = List.merge by_priority [(key,priority)] pqueue

let pop pqueue =
  match pqueue with
    | [] -> None
    | front::rest -> Some (front,rest)

let to_list pqueue = pqueue

let split_at_length n rs =
  let rec loop n ls rs =
    if n <= 0
    then Some (Dlist.to_list ls, rs)
    else
      match rs with
        | [] -> None
        | r::rs -> loop (n-1) (Dlist.snoc r ls) rs in
  loop n Dlist.empty rs

let split_at_priority priority rs =
  let rec loop ls rs =
    match rs with
      | [] -> (Dlist.to_list ls, rs)
      | (r,p)::rs ->
          if p < priority
          then loop (Dlist.snoc (r,p) ls) rs
          else (Dlist.to_list ls, rs) in
  loop Dlist.empty rs