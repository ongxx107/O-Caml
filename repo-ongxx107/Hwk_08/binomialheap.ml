(*
Binomial Tree Invariants
1. Tree with rank r is a node consists of subtrees (tree left and tree right)
   with rank (r-1) for each subtree.
2. Parent node value must be smaller than child node value such that the
   binomial tree is under minimum heap property.
3. Each child node is structured in ascending order from left to right such that
   the left child node left is always smaller than right child node.

Binomial Heap Invariants
1. There is no 2 tree in the binomial heap list has same rank.
2. Heap sorts the tree list in ascending order compared with each tree rank.
*)
open Ordered

module type BinomialHeapSig = sig
  type elem
  type tree = Node of int * elem * tree list
  type t = tree list

  val empty: t
  val isEmpty: tree list -> bool

  val insert: elem -> tree list -> tree list
  val merge: tree list -> tree list -> tree list

  val findMin: tree list -> elem
  val deleteMin: tree list -> tree list
  val findMinDirect: t -> elem

  val isBinomialTree: tree -> bool
  val isBinomialHeap: t -> bool
end

module BinomialHeap (O: OrderedSig): (BinomialHeapSig with type elem = O.t)
= struct
    type elem = O.t
    type tree = Node of int * elem * tree list
    type t = tree list

    let empty = []
    let isEmpty t =
      match t with
      | [] -> true
      | _ -> false

    let rank (Node (r, x, c)) = r
    let root (Node (r, x, c)) = x

    let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
      if O.leq x1 x2 then Node (r+1, x1, t2::c1)
      else Node (r+1, x2, t1::c2)
    let rec insTree t ts =
      match ts with
      | [] -> [t]
      | t'::ts' -> if rank t < rank t' then t::ts
                   else insTree (link t t') ts'

    let insert x ts = insTree (Node (0, x, [])) ts

    let rec merge ts1 ts2 =
      match ts1, ts2 with
      | _, [] -> ts1
      | [], _ -> ts2
      | t1::ts1', t2::ts2' ->
        if rank t1 < rank t2 then t1 :: (merge ts1' ts2)
        else if rank t2 < rank t1 then t2 :: (merge ts1 ts2')
        else insTree (link t1 t2) (merge ts1' ts2')


    let rec removeMinTree tr =
      match tr with
      | [] -> raise(Failure "Empty")
      | [t] -> (t, [])
      | t::ts ->
        let t',ts' = removeMinTree ts
        in
        if O.leq (root t) (root t') then (t, ts) else (t', t::ts')

    let findMin ts =
      let t, _ = removeMinTree ts in root t

    let deleteMin ts =
      let Node (_, x, ts1), ts2 = removeMinTree ts
      in
      merge (List.rev ts1) ts2

    let findMinDirect tr =
      let rec helper tr =
        match tr with
        | [] -> raise (Failure "empty")
        | [x] -> root x
        | x::xs ->
                    if O.leq (root x) (helper xs) then (root x)
                    else helper xs


      in helper tr

    let isBinomialTree t =
      let rec helper t =
        match t with
        | Node (0, v, []) -> true
        | Node (_, v, []) -> false
        | Node (r, v, c::cs) -> if r = ((rank c)+1) && O.leq v (root c) then
                                (
                                  match cs with
                                  | [] -> true
                                  | Node (_, _, [])::[] -> true
                                  | Node (r1, v1, any::[])::[]
                                    when rank c = (r1+1) && O.leq (root c) v1
                                    -> true && helper any
                                  | _ -> raise (Failure "Only one Nodeeeeee")
                                )
                                else false
        | _ -> raise (Failure "Only one Node")

      in helper t

    let isBinomialHeap tLst =

      let rec helper tLst =
        match tLst with
        | [] -> true
        | _ -> true
        | x1::x2::xs -> if (rank x1) < (rank x2) && isBinomialTree x1 then
                        true && helper (x2::xs) else false
      in helper tLst

end

module BHI = BinomialHeap(Int)

let h1 = BHI.empty
let h2 = BHI.insert 20 h1
let h3 = BHI.insert 30 h2
let h4 = BHI.insert 10 h3
let h5 = BHI.insert 40 h4

let m1 = BHI.findMin h5

let h6 = BHI.deleteMin h5

let m2 = BHI.findMin h6

let m3 = BHI.findMinDirect h6

let t1 = BHI.Node (1, 20, [BHI.Node (0, 30, [])])

let test = BHI.isBinomialTree t1

let test2 = BHI.isBinomialHeap h2
let test3 = BHI.isBinomialHeap h3
let test4 = BHI.isBinomialHeap h4
let test5 = BHI.isBinomialHeap h5
