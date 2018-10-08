open LazeeModules
open StreamModules


module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream
end

module Hwk5(S: StreamSig) : Hwk5Sig = struct
    type 'a stream = 'a S.t
    let delay = S.delay
    let demand = S.demand
    let zip = S.zip
    let head = S.head
    let tail = S.tail
    let take = S.take
    let map = S.map
    let filter = S.filter

   (* add elements here to complete the functor *)

   let rec from (n: int): int stream =
       S.Cons ( n,
            delay (fun () -> from (n+1) )
         )

   let nats: int stream = from 1

   let rec cubes_from v : int stream =
       S.Cons (v * v * v, delay (fun () -> cubes_from (v+1)))

   let cubes_from_zip (x : int) : int stream =
       let square = zip ( * ) (from x) (from x)
       in
       zip ( * ) square (from x)

   let cubes_from_map (x : int) : int stream =
       map (fun x -> x * x * x) (from x)

   let rec drop (i: int) (s: 'a stream) : 'a stream =
       if i = 0 then s          (* base case *)
       else drop (i-1) (tail s) (* recursion *)

   let rec drop_until (f: 'a -> bool) (s: 'a stream) : 'a stream =
       if f (head s) then s
       else drop_until f (tail s)

   let rec foldr (f: 'a -> 'b S.lazee -> 'b) (s: 'a stream) : 'b =
       match s with
       | S.Cons (hd, tl) ->
           f (head s) ( delay (fun () -> foldr f (tail s)) )

   let rec and_fold (s: bool stream) : bool =
       foldr ( fun x y -> x && demand y ) s

   let sum_positive_prefix (s: 'a stream) : int =
       foldr (fun x y -> if x>0 then x+(demand y) else 0) s

   let sift (i: int) (s: int stream): int stream =
       let modd x y =
           y mod x <> 0
       in
       filter (modd i) s

   let rec sieve (s: int stream) : int stream =
       match s with
       | S.Cons (hd, tl) -> S.Cons (hd, delay( fun () -> sieve (sift hd (tail s) ) ) )

   let primes = sieve (from 2)

end
