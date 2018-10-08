type state = int * int
(* 
Question 1.
I use int type for the type state which is a tuple in water_jug.ml. 
In the tuple of two integers, we have to represent 4 gallon in the 
first argument of tuple and 3 gallon in the second argument of tuple
at any state.   
*)

type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug

let ok_state ( (four, three): state) : bool =
  (four <= 4 && four >= 0) || (three <=3 && three >=0)

let final s =
  match s with
  | (four, three) -> if four = 2 then true else false

let moves (s : state) : (operation * state) list =
  let fill4Tap (four, three) =
    if four < 4 then [( Fill4GallonJugFromTap, (4, three) )] else []
  in

  let fill3Tap (four, three) =
    if three < 3 then [( Fill3GallonJugFromTap, (four, 3) )] else []
  in

  let empty4Ground (four, three) =
    if four <> 0 then [( Empty4GallonJugOnGround, (0, three) )] else []
  in

  let empty3Ground (four, three) =
    if three <> 0 then [( Empty3GallonJugOnGround, (four, 0) )] else []
  in

  let fill4Jug (four, three) =
    if four <> 0 && four+three >= 4 && three > 0 then
      let remainder = 4 - four
      in
      [( Fill4GallonJugFrom3GallonJug, (4, three - remainder) )] else []
  in

  let fill3Jug (four, three) =
      if three <> 0 && four+three >= 3 && four>0 then
      let remainder = 3 - three
      in
      [( Fill3GallonJugFrom4GallonJug, (four-remainder, 3) )] else []
  in

  let empty4Jug (four, three) =
    if four+three <= 3 && four>0 then
    [( Empty4GallonJugInto3GallonJug, (0, four+three) )] else []
  in

  let empty3Jug (four, three) =
    if four+three <= 4 && three>0 then
    [( Empty3GallonJugInto4GallonJug, (four+three, 0) )] else []
  in

   ( fill3Tap s @ fill3Jug s @
     empty3Ground s @ empty3Jug s @
     empty4Ground s @ empty4Jug s @
     fill4Tap s @ fill4Jug s )

let rec is_not_elem (path: (operation * state) list) (op, st) : bool =
    match path with
    | [] -> true
    | (op1, st1)::rest -> 
      if st1 = st then false else true && is_not_elem rest (op, st)

let describe (four:int) (three:int) : string =
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

let rec finalRes lst =
          match lst with
          |[] -> []
          |(op, (four, three) )::rest -> 
            [(op,describe four three)] @ finalRes rest

let play (u: unit): (operation * string) list option =
    let rec go_from state path =
    if final state
    then Some (finalRes path)
    else
      match List.filter (is_not_elem path) (moves state) with
        |[] -> None
        |[(op, st)] -> go_from st (path @ [(op, st)])
        |[(op1, st1);(op2, st2)] ->
          (
            match go_from st1 (path @ [(op1, st1)]) with
            | Some path' -> Some path'
            | None -> go_from st2 (path @ [(op2, st2)])
          )
         |[(op1, st1);(op2, st2);(op3, st3)] ->
           (
            match go_from st1 (path @ [(op1,st1)]) with
            | Some path' -> Some path'
            | None -> ( match go_from st2 (path @ [(op2, st2)]) with
                      | Some path'' -> Some path''
                      | None -> go_from st3 (path @ [(op3, st3)])
                      )
           )
        |_ -> raise (Failure ("No more than 3 states"))
      in

      go_from (0, 0) []
