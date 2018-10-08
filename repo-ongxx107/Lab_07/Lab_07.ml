# Problem 1
let rec length = function
  | [] -> 0
  | x::xs -> 1 + length xs

P(l, r) = length (l @ r) = length l + length r

Induction on l
Base:
    P([], r) = length ([]@r)
             = length r       # by def of @
             = 0 + length r   # by def of arithmetic
             = length [] + length r # by def of length


show: P(x::xs, r) = length (x::xs @ r) = length x::xs + length r
given:    length (xs@r) = length xs + length r


Inductive Case:
    P(x::xs, r) = length (x::xs@r)
                = length (([x] @ xs) @ r)    # by understanding of :: and @
                = length ([x] @ (xs@r))      # by understanding of list and @
                = length (x :: (xs@r))       # by understanding of list and @
                = 1 + length (xs@r)          # by def of length
                = 1 + length xs + length r   # by inductive hypothesis
                = (1 + length xs) + length r # by arithmetic
                = length x::xs + length r    # by def of length



# Problem 2
let rec reverse l = match l with
  | [ ] -> [ ]
  | x::xs -> reverse xs @ [x]

show: P(l) = length (reverse l) = length l

Induction on l
Base:
    P([]) = length (reverse [])
          = length []            # by def of reverse
          = 0                    # by def of length


show: P(x::xs) = length (reverse x::xs) = length x::xs
given: P(xs) = length(reverse xs) = length xs


let rec length = function
  | [] -> 0
  | x::xs -> 1 + length xs
P(l, r) = length (l @ r) = length l + length r
Inductive case:
    P(x::xs) = length (reverse x::xs)
             = length (reverse xs@[x])  # by def of reverse
             = length (reverse xs) + length [x]   # by def of length
             = length xs + length [x]             # by inductive hypothesis
             = length xs + length x::[]             # by def of length
             = length xs + 1 + length []              # by def of length
             = length xs + 1 + 0              # by def of length
             = 1 + length xs                  # by arithmetic
             = length x::xs                   # by def of length






#lemma: append c v = c @ v
