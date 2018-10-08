(* The code below is from Professor Eric Van Wyk. *)

(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee

(* The code below is from Ren Jeik Ong *)

let rec cubes_from v : int stream =
    Cons (v * v * v, delay (fun () -> cubes_from (v+1)))

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) (demand tl2)))

let ones: int stream =
    let rec ones_h () =
        Cons (1, delay ones_h)
    in ones_h ()

let rec from n =
    Cons ( n,
            delay (fun () -> from (n+1) )
         )

let nats = from 1

let head (s: 'a stream) : 'a = match s with
    | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
    | Cons (_, tl) -> demand tl

let rec take (n:int) (s : 'a stream) : ('a list) =
    match n, s with
    | 0, _ -> []
    | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

let cubes_from_zip (x : int) : int stream =
    let square = zip ( * ) (from x) (from x)
    in
    zip ( * ) square (from x)

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
    match s with
    | Cons (hd, tl) ->
        Cons (f hd, delay (fun () -> map f (demand tl)))

let cubes_from_map (x : int) : int stream =
    map (fun x -> x * x * x) (from x)

let rec drop (i: int) (s: 'a stream) : 'a stream =
    if i = 0 then s          (* base case *)
    else drop (i-1) (tail s) (* recursion *)

let rec drop_until (f: 'a -> bool) (s: 'a stream) : 'a stream =
    if f (head s) then s
    else drop_until f (tail s)

(*
First argument is a must because it is an output for that function.
However, the 2nd argument is not neccessary to be used. Therefore,
we use lazee type(call by name) for 2nd argument since we might need to
use it later. In foldr function, we start pattern matching with the stream,
the hd is first argument in lambda function. Later, 2nd argument which is lazee
stream is being delayed until everything is done recursively.
*)
let rec foldr (f: 'a -> 'b lazee -> 'b) (s: 'a stream) : 'b =
    match s with
    | Cons (hd, tl) ->
        f (head s) ( delay (fun () -> foldr f (tail s)) )

let rec and_fold (s: bool stream) : bool =
    foldr ( fun x y -> x && demand y ) s

let sum_positive_prefix (s: 'a stream) : int =
    foldr (fun x y -> if x>0 then x+(demand y) else 0) s

let ns : int stream = zip ( - ) (from 1000) (cubes_from 1)
let are_positive ns = map (fun n -> n > 0) ns
let ns_positive : bool stream = are_positive ns

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
    match s with
    | Cons (hd, tl) ->
        let rest = delay (fun () -> filter p (demand tl)) in
        if p hd
        then Cons (hd, rest)
        else demand rest

let sift (i: int) (s: int stream): int stream =
    let modd x y =
        y mod x <> 0
    in
    filter (modd i) s

let rec sieve (s: int stream) : int stream =
    match s with
    | Cons (hd, tl) -> Cons (hd, delay( fun () -> sieve (sift hd (tail s) ) ) )

let primes = sieve (from 2)
