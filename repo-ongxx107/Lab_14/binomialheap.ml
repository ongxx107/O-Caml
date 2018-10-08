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
        | x1::x2::xs -> (match x1, x2 with
                  | Node(r1, v1, c1), Node(r2, v2, c2) ->
                    if O.leq v1 v2 then helper x1::xs
                    else  helper x2::xs

                  )
      in helper tr
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
