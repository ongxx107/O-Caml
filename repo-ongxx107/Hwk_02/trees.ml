type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree

let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))

let rec t_size  ( t :'a tree) : int =
    match t with
    | Leaf e -> 1
    | Fork(e, tl, tr) -> 1 + t_size(tl) + t_size(tr)

let rec t_sum (t : int tree) : int =
  match t with
  | Leaf e -> e
  | Fork(e, tl, tr) -> e + t_sum(tl) + t_sum(tr)

let rec t_charcount (t : string tree) : int =
  match t with
  | Leaf s -> String.length s
  | Fork (s, tl,tr) -> String.length s + t_charcount tl + t_charcount tr

let rec t_concat (t : string tree) : string =
  match t with
  | Leaf s -> s
  | Fork (e, tl, tr) -> e ^ t_concat tl ^ t_concat tr

let t5 : string option tree =
  Fork (Some "a",
        Leaf (Some "b"),
        Fork (Some "c",
              Leaf None,
              Leaf (Some "d")))

let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))

let rec t_opt_size (t: 'a option tree) : int =
  match t with
  | Leaf (Some e) -> 1
  | Leaf (None) -> 0
  | Fork (Some e,  tl, tr) -> 1 + t_opt_size(tl) + t_opt_size( tr)
  | Fork (None,  tl, tr) -> t_opt_size(tl) + t_opt_size( tr)

let rec t_opt_sum (t: int option tree) : int =
  match t with
  | Leaf (Some e) -> e
  | Leaf None -> 0
  | Fork (Some e, tl, tr) -> e + t_opt_sum (tl) + t_opt_sum (tr)
  | Fork (None, tl, tr) -> t_opt_sum (tl) + t_opt_sum (tr)

let rec t_opt_charcount (t: string option tree) : int =
  match t with
  | Leaf (Some e) -> String.length e
  | Leaf None -> 0
  | Fork (Some e, tl, tr) -> String.length e + t_opt_charcount tl + t_opt_charcount tr
  | Fork (None, tl, tr) -> t_opt_charcount tl + t_opt_charcount tr

let rec t_opt_concat (t: string option tree) : string =
  match t with
  | Leaf (Some e) -> e
  | Leaf (None)-> ""
  | Fork (Some e, tl, tr) -> e ^ t_opt_concat tl ^ t_opt_concat tr
  | Fork (None, tl, tr) -> t_opt_concat tl ^ t_opt_concat tr

let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b =
  match t with
  | Leaf v -> l v
  | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_size (t : 'a tree) : int =
  tfold (fun l -> 1) (fun x result_of_l result_of_r-> 1 + result_of_l + result_of_r) t

let tf_sum (t : int tree) : int =
  tfold (fun l -> l) (fun x l r -> x+l+r) t

let tf_charcount (t: string tree) : int =
  tfold (fun l -> String.length l) (fun x l r -> String.length x + l + r) t

let tf_concat (t: string tree) : string =
  tfold (fun l -> l) (fun x l r -> x ^ l ^ r) t

let tf_opt_size (t : 'a option tree) : int =
  tfold (fun l ->
  match l with
  |Some x -> 1
  |None -> 0 ) (fun x result_of_l result_of_r -> 1 + result_of_l + result_of_r) t

let tf_opt_sum (t : int option tree) : int =
  tfold (fun l ->
  match l with
  |Some x -> x
  |None -> 0 )
  (fun x l r ->
  match x with
  | Some x -> x+l+r
  | None ->l + r) t

let tf_opt_charcount (t: string option tree) : int =
  tfold (fun l ->
  match l with
  |Some x -> String.length x
  | None -> 0)
  (fun x l r ->
  match x with
  | Some x -> String.length x + l + r
  | None -> l+r ) t

let tf_opt_concat (t: string option tree) : string =
  tfold (fun l ->
  match l with
  | Some x -> x
  | None -> "")
  (fun x l r ->
   match x with
   | Some x -> x ^ l ^ r
   | None -> l^r) t

type 'a btree = Empty
  | Node of 'a btree * 'a * 'a btree

let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))

let helper x y= if x=y then 0  else if x>y then 1 else -1
let rec bt_insert_by compare (element: 'a) ( t: 'a btree) : 'a btree=
  match t with
  | Empty ->  Node(Empty, element, Empty)

  | Node (l, e, r) -> if compare element e = 1 then
  Node(l, e, bt_insert_by compare element r)
  else Node(bt_insert_by compare element l, e, r)

let helper2 x y = if compare x y = 0 then true else false
let rec bt_elem_by (f: 'a -> 'b -> bool) (element : 'b) (t: 'a btree) : bool =
  match t with
  | Empty -> false
  | Node(l, e, r) -> if f e element then true
  else if compare element e = 1 then
  bt_elem_by f element r
  else
  bt_elem_by f element l

let rec bt_to_list (t: 'a btree) : 'a list =
  match t with
  | Empty -> []
  | Node(l, e, r) -> bt_to_list l@[e]@bt_to_list r

let rec btfold (l:'b) (f:'b -> 'a -> 'b -> 'b) (t:'a btree) : 'b =
  match t with
  | Empty -> l
  | Node(left, e, right) -> f (btfold l f left) e (btfold l f right)

let btf_elem_by (f: 'a -> 'b -> bool) (element : 'b) (t: 'a btree) : bool =
  btfold false (fun l e r -> if f e element || r || l  then true else false) t

let btf_to_list (t: 'a btree) : 'a list =
  btfold [] (fun l e r -> l@[e]@r) t

(* btf_insert_by is hard to build because it has new inductive types which are Leaf and Node.*)
(* Since it is binary tree, you have to choose which subtree either left or right that you should traverse to *)
