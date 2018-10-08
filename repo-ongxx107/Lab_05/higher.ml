
let all_evens lst : int list= List.filter (fun x -> x mod 2=0) lst;;

let increment_all lst: int list = List.map (fun x -> x+1) lst;;

let max_fold lst : int =
  match lst with
  | [] -> raise (Failure "Input list must not be empty")
  | [x1] -> x1
  | all -> List.fold_left (fun x y-> if x> y then x else y)
    (List.hd ( List.sort compare lst) ) all;;

let sum_prod lst:(int*int) =
  match lst with
  | [] -> (0, 1)
  | [x1] -> (x1, x1)
  | all -> (List.fold_left ( + ) 0 lst, List.fold_left ( * ) 1 lst);;


let split (f: 'a -> bool) (lst: 'a list) : 'a list list = (*add the input and output type*)
(*This functions uses the function argument and splits up the list into sublists*)
  let accum = ([],[])
  in
  let helper (result1,result2) currentElement =
    if f currentElement then (result1@[result2], [])
    else (result1, result2@[currentElement])
  in
  let (result1, result2) = List.fold_left helper accum lst
  in
  result1@[result2]
