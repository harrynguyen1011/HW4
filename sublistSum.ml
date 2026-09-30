(*
Harry Nguyen
Huy Phan
*)

(*
sum : int list -> int
REQUIRES: true
ENSURES: sum (lst) -*-> the sum of all the elements in the list
*)
let rec sum (lst : int list) : int = 
  match lst with 
  | [] -> 0
  | x :: lst' -> x + sum (lst')

(*
add_to_all_sls : int -> int list list -> int list list
REQUIRES: true
ENSURES: add_to_all_sls (x) (sls) -*-> add the element x to all the sublists int the list of sublists
*)
let rec add_to_all_sls (x: int) (sls: int list list) : int list list =
  match sls with
  | [] -> []
  | s :: sls' -> (x::s) :: add_to_all_sls (x) (sls')

(*
allSublists : int list -> int list list
REQUIRES: true
ENSURES: allSublists lst -∗→ sls such that sls contains all sublists of sls
*)
let rec allSubLists (lst: int list) : int list list =
  match lst with
  | [] -> [[]]
  | x :: lst' -> let rest = allSubLists (lst') in add_to_all_sls (x) (rest) @ rest

(*
sub_lists_sum_helper: int list list -> int -> int list option
REQUIRES: true
ENSURES: sub_lists_sum_helper (sls) (n) -*-> Some lst where sum (lst) = n and lst is a sublist of the original list
*)
let rec sub_lists_sum_helper (sls: int list list) (n : int) : int list option = 
  match sls with
  | [] -> None
  | lst :: sls' -> 
    begin
      if sum (lst) = n 
      then Some lst
      else sub_lists_sum_helper (sls') (n)
    end

(*
sublistSum : int list * int -> int list option
REQUIRES: true
ENSURES: sublistSum ( lst , n ) -∗→ Some lst ’ where lst ’ ⊑ lst and sum
lst ’ ∼= n.
sublistSum ( lst , n ) -∗→ None if there is no such sublist lst ’.
*)
let rec sublistSum ( (lst : int list), (n : int) ) : int list option = 
  let sls = allSubLists (lst) in 
  sub_lists_sum_helper (sls) (n)
