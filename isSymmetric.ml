(*
Harry Nguyen
Huy Phan
*)
type tree = Empty | Node of tree * int * tree

(*
invert : tree -> tree
REQUIRES: true
ENSURES: invert (t) -*-> create a mirror image of tree t
*)
let rec invert (t : tree) : tree = 
  match t with
  | Empty -> Empty
  | Node (tl, x, tr) -> Node (invert tr, x, invert tl)

(*
is_symmetric_helper : tree -> tree -> bool
REQUIRES: true
ENSURES: is_symmetric_helper (t1, t2) -*-> true iff invert t can be obtained from t by a series
of changes to node values, and false otherwise
*)
let rec is_symmetric_helper (t1: tree) (t2: tree) : bool = 
  match t1, t2 with 
  | Empty, Empty -> true
  | Empty, _ -> false
  | _, Empty -> false
  | Node (tl1, x1, tr1), Node(tl2, x2, tr2) -> is_symmetric_helper (tl1) (tl2) && is_symmetric_helper (tr1) (tr2)
  
(*
isSymmetric : tree -> bool
REQUIRES: true
ENSURES: isSymmetric t -∗→ true iff invert t can be obtained from t by a series
of changes to node values, and false otherwise
*)
let isSymmetric (t : tree) : bool = 
  let inverted_t = invert (t) in
  is_symmetric_helper (t) (inverted_t)

