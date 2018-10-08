open Intervals

module Rational_comparable : (Comparable with type t = (int * int)) = struct
  type t = (int * int)
  let compare (v1, v2) (i1, i2)= compare (v1*i2) (v2*i1)

  let to_string (a, b) =
    let rec euclid a b:int=
      if a<b
      then euclid a (b-a)
      else if a>b
      then euclid (a-b) b
      else
      b
    in
    let frac_simplify (x,y)=
      let num = euclid x y
      in
      (x/num, y/num)
    in
    let (x1, x2) = frac_simplify (a, b)
    in
    string_of_int x1 ^ "/" ^ string_of_int x2
end

module Rational_interval = Make_interval(Rational_comparable)


(* The following line now works. *)
let i = Rational_interval.create (3, 4)
