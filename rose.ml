(*
Harry Nguyen
Huy Phan
*)
type rose = Rose of int * rose list

(*
sumUp : rose -> int
REQUIRES: true
ENSURES: sumUp t -∗→ n where n is the total of all numbers held in t
*)
let rec sumUp (t : rose) : int =
  match t with
  | Rose (v, children) -> v + sum_up_helper children

(*
sum_up_helper : rose list -> int
REQUIRES: true
ENSURES: sum_up_helper children -*-> n where n is the total of all numbers held in
         every rose tree in children
*)
and sum_up_helper (children : rose list) : int =
  match children with
  | [] -> 0
  | child :: children' -> sumUp child + sum_up_helper children'
