(*
Red Black Tree Invariants
1. No red node has a red child in the binary tree.
2. Every path from root to every leaf node have the same number of black nodes.
3. Red Black Tree is advanced version of balanced binary search tree. Thus, it
   must be a treee that is in sorted order.
4. Leaf node and root node must be black node in this tree.

*)

open Ordered

module type RedBlackSetSig = sig
  type elem
  type color = R | B
  type t = E | T of color * t * elem * t

  val empty: t
  val insert: elem -> t -> t
  (* val balance: t -> t *)
  val member: elem -> t -> bool

  val isRedBlackTree: t -> bool

end

module RedBlackTree (O: OrderedSig): (RedBlackSetSig with type elem = O.t)
= struct
    type elem = O.t
    type color = R | B
    type t = E | T of color * t * elem * t

    let empty = E

    let rec member (x: elem) (tree: t): bool =
      match tree with
      | E -> false
      | T (_, a, y, b) -> if O.lt x y then member x a
                        else if O.lt y x then member x b
                        else true

    let balance c t1 v t2 =
      match c, t1, v, t2 with
      | B, T (R, T (R, a, x, b), y, c), z, d
      | B, T (R, a, x, T (R, b, y, c)), z, d
      | B, a, x, T (R, T (R, b, y, c), z, d)
      | B, a, x, T (R, b, y, T (R, c, z, d))
      -> T (R, T (B, a, x, b), y, T (B, c, z, d))
      | anyColor, anyT1, anyV, anyT2 -> T (anyColor, anyT1, anyV, anyT2)

    let insert (x: elem) (tree: t): t =
      let rec ins s =
        match s with
        | E -> T (R, E, x, E)
        | T (color, a, y, b) -> if O.lt x y then balance color (ins a) y b
                               else if O.lt y x then balance color a y (ins b)
                               else s
      in
       match ins tree with
      | E -> raise ( Failure "Empty" )
      | T (_, a, y, b) -> T (B, a, y, b)

    let rec getColor t =
      match t with
      | E -> B
      | T (c, a, y, b) -> if c = R then R else B

    let rec isValidChild t =
      match t with
      | E -> true
      | T (R, a, y, b) -> if getColor a = R || getColor b = R then false
                          else isValidChild a && isValidChild b
      | T (B, a, y, b) -> isValidChild a && isValidChild b

    let rec checkNumBlack t accum finalValue =
      match t with
      | E -> finalValue::accum
      | T (B, a, y, b) -> (checkNumBlack a accum (finalValue+1))
                          @ (checkNumBlack b accum (finalValue+1))
      | T (R, a, y, b) -> (checkNumBlack a accum finalValue)
                          @ (checkNumBlack b accum finalValue)

    let validBlack lst =
      let finalLst = checkNumBlack lst [] 0
      in

      let rec helper l =
        match l with
        | [] -> true
        | x::[] -> true
        | x1::x2::xs -> if x1 = x2 then true && helper (x2::xs) else false
      in

      helper finalLst

    let getValue t ptr =
      match t with
      | E -> ptr
      | T (c, a, y, b) -> y

    let rec isBST t =
      match t with
      | E -> true
      | T (c, a, y, b) -> if O.leq (getValue a y) y && O.leq y (getValue b y)
                          then true && isBST a && isBST b else false

    let isRedBlackTree t =
      validBlack t && isValidChild t && isBST t


end

module RBTI = RedBlackTree (Int)

let h1 = RBTI.empty
let h2 = RBTI.insert 20 h1
let h3 = RBTI.insert 30 h2
let h4 = RBTI.insert 10 h3
let h5 = RBTI.insert 40 h4
let h6 = RBTI.insert 50 h5

let test2 = RBTI.isRedBlackTree h2
let test3 = RBTI.isRedBlackTree h3
let test4 = RBTI.isRedBlackTree h4
let test5 = RBTI.isRedBlackTree h5
let test6 = RBTI.isRedBlackTree (RBTI.T (RBTI.B,
RBTI.T (RBTI.R, RBTI.E, 40, RBTI.E), 20, RBTI.T (RBTI.R, RBTI.E, 30, RBTI.E)))
