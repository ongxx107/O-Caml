
let all_evens lst = List.filter (fun x -> x mod 2=0) lst;;

let increment_all lst = List.map (fun x -> x+1) lst;;

let max_fold lst:int =
  match lst with
  | [] -> raise (Failure "Input list must not be empty")
  | [x1] -> x1
  | all -> List.fold_left (fun x y-> if x> y then x else y) (List.hd ( List.sort compare lst) ) all;;

let sum_prod lst:(int*int) =
  match lst with
  | [] -> (0, 1)
  | [x1] -> (x1, x1)
  | all -> (List.fold_left ( + ) 0 lst, List.fold_left ( * ) 1 lst);;


let split f lst =
  let helper (x1,x2) x =
    if f x then (x1@[x2], []) else (x1, x2@[x])
  in
  let (t1, t2) = List.fold_left helper ([],[]) lst
  in 
  t1@[t2] 

