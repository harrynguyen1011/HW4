type tree = Empty | Node of tree * int * tree

let rec invert (t : tree) : tree = 
  match t with
  | Empty -> Empty
  | Node (tl, x, tr) -> Node (invert tr, x, invert tl)

let rec is_symmetric_helper (t1: tree) (t2: tree) : bool = 
  match t1, t2 with 
  | Empty, Empty -> true
  | Empty, _ -> false
  | _, Empty -> false
  | Node (tl1, x1, tr1), Node(tl2, x2, tr2) -> is_symmetric_helper (tl1) (tl2) && is_symmetric_helper (tr1) (tr2)
  

let isSymmetric (t : tree) : bool = 
  let inverted_t = invert (t) in
  is_symmetric_helper (t) (inverted_t)

