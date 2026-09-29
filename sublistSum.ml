let rec sum (lst : int list) : int = 
  match lst with 
  | [] -> 0
  | x :: lst' -> x + sum (lst')

let rec add_to_all_sls (x: int) (sls: int list list) : int list list =
  match sls with
  | [] -> []
  | s :: sls' -> (x::s) :: add_to_all_sls (x) (sls')

let rec allSubLists (lst: int list) : int list list =
  match lst with
  | [] -> [[]]
  | x :: lst' -> let rest = allSubLists (lst') in add_to_all_sls (x) (rest) @ rest
  
let rec sub_lists_sum_helper (sls: int list list) (n : int) : int list option = 
  match sls with
  | [] -> None
  | lst :: sls' -> 
    begin
      if sum (lst) = n 
      then Some lst
      else sub_lists_sum_helper (sls') (n)
    end

let rec sublistSum ( (lst : int list), (n : int) ) : int list option = 
  let sls = allSubLists (lst) in 
  sub_lists_sum_helper (sls) (n)