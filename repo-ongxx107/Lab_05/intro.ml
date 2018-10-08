let even x : bool=
  if x mod 2 ==0
  then true
  else false;;

let rec euclid a b:int=
  if a<b
  then euclid a (b-a)
  else if a>b
  then euclid (a-b) b
  else
  b;;

let frac_simplify (x,y): (int*int)=
  let num = euclid x y
  in
  (x/num, y/num);;

let rec max lst:int =
  match lst with
  | [] -> raise (Failure "Input list must not be empty")
  | [x1] -> x1
  | x1::x2::rest  when x2>x1 -> max (x2::rest)
  | x1::x2::rest  -> max (x1::rest);;

let rec take (x: int) (list: 'a list): 'a list=
  if x<=0 then []
  else
  match list with
  | [] when x<=0 -> []
  | i::[] when x=1 -> [i]
  | i::rest -> [i]@ (take (x-1) rest)  ;;
