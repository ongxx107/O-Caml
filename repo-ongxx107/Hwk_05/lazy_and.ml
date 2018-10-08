(* Ren Jeik Ong (ongxx107) *)
let rec ands (lst: bool list) =
    match lst with
    | [] -> true
    | x::xs -> if x then ands xs else false
