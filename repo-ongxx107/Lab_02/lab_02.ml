let circle_circum_v1 radius=
	2.0 *. 3.1415 *. radius;;

let circle_circum_v2 radius=
	let pi = 3.1415
	in
	2.0 *. pi *. radius;;

let rec product xs =
	match xs with
	| [] -> 1
	| x::rest -> x * product rest;;

let rec sum_sqrdiffs xs =
  match xs with
	| x1::(x2::[]) -> (x1 - x2) * (x1 - x2)
	| x1::(x2::rest) -> (x1 - x2) * (x1 - x2) + sum_sqrdiffs (x2::rest);;

let distance (x1,y1) (x2,y2) =
	sqrt((x2-.x1)*.(x2-.x1)+.(y2-.y1)*.(y2-.y1));;

let triangle_perimeter (x1,y1) (x2,y2) (x3,y3) =
  let a = distance (x1,y1) (x2,y2)
  in
  let b = distance (x2,y2) (x3,y3)
  in
  let c = distance (x1,y1) (x3,y3)
  in
  a+.b+.c;;
