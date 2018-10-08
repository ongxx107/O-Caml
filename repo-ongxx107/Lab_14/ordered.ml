
module type OrderedSig = sig
  type t
  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val leq: t -> t -> bool
end

module Int: (OrderedSig with type t = int ) = struct
  type t = int
  let empty = []
  let eq x1 x2 = if x1 = x2 then true else false
  let lt x1 x2 = if x1 < x2 then true else false
  let leq x1 x2 = if x1 <= x2 then true else false
end
