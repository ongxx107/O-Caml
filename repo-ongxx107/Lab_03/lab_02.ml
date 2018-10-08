(*Leqi Zhang & Ren Jeik Ong*)
(* TODO *)

let circle_circum_v1 radius = 2.0 *. 3.1415 *. radius
(*There is only one line for this function. *)
(* Yes, since it's a simple formula of 2*pi*r which calculates the circle's circumference.*)
(*Yes, it does. *)

let circle_circum_v2 radius=
	let pi = 3.1415
	in
	2.0 *. pi *. radius
(*It uses four lines. *)
(*Yes, there are indentations from line 2 to 4. *)
(*Each of them contains one tab only.*)
(*Yes, because of indentation of 'let pi= 3.1415' and 'in', their styling are easy to see as they are in different line. *)

let rec product xs =
	match xs with
	| [] -> 1
	| x::rest -> x * product rest
(*No warming is showed. *)
(*Because it has base case of '[]-> 1'.*)

let rec sum_sqrdiffs xs =
  match xs with
	| x1::(x2::[]) -> (x1 - x2) * (x1 - x2)
	| x1::(x2::rest) -> (x1 - x2) * (x1 - x2) + sum_sqrdiffs (x2::rest)
	| _ -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
(*No, my code does not use raise construct. *)
(*No, there is no append operator '@' in my recursive call. *)


let distance (x1,y1) (x2,y2) = sqrt((x2-.x1)*.(x2-.x1)+.(y2-.y1)*.(y2-.y1))
(*I prefer the second operator because it's easy to understand the meaning of *.(multiple) is possibly know by the user*)

let triangle_perimeter v1 v2 v3 =
  (distance v1 v2) +. (distance v2 v3) +. (distance v3 v1)

(*Yes, my triangle_perimeter function uses distance function. *)
(*Yes*)
(*Yes. It would be better if the syntax is ^ instead of **. *)
