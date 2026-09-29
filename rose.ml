type rose = Rose of int * rose list
type rose2 = Rose of int * rose2list
and rose2list = Nil | Cons of rose2 * rose2list

let rec sumUp (t : rose) : int =
  match t with
  | Rose (v, children) -> v + sum_up_helper children

and sum_up_helper (children : rose list) : int =
  match children with
  | [] -> 0
  | child :: children' -> sumUp child + sum_up_helper children'
