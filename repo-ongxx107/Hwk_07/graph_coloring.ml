(* 
Question 1.
Search space that my solution explores is coloring. At each step, 
it searches from matching the graph and call the helper function with 
node list as argument. If the node list is empty then it returns None
else it goes through recursive calls and start assigning the color with
current node. If your current node color matches the next node color, 
then the isValidColor function will returns false and it will find 
other color for the next node in other match case. Then, it returns 
None when the coloring is not valid which means the color is wrong, 
eventually it will moves to match with new color (C 2).
If the validation returns true, it will return Some coloring and 
then continue the recursive calls for those unvisited nodes in the 
'rest' list and start assigning the color C 1. Same process happen 
again, if the helper function returns true in the sense that the 
neighboring nodes have different colors, then helper function will go 
through recursion for the rest nodes. Otherwise, if the validation is 
false, then it goes to None and jumps to new color C2. If the new color 
C 2 fails the validation, then it will goes to new color C 3 recursively.
The worst case is that there is no suitable color for that node and it 
will returns None.

Question 2.
My solution avoids this potential inefficiency because if the validation
fails on C 1 which means the neighboring nodes have same colors, then it 
won't add the same Color because it returns None and start a helper 
function call with new color such as C 2. This funciton will be run 
continuely accumulated the coloring list instead of starting the coloring 
search all over again and creating a new coloring list which has low 
optimization. On ther other hand, if the neigboring nodes have different 
colors, then it returns that result wrapped up in a Some.
*)

type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list

let rec getColor n coloring : color option=
    match coloring with
    | [] -> None
    | (nde, color)::rest -> if nde = n then Some color else getColor n rest

let rec isValidColor (g:graph) (coloring:coloring): bool =
    match g with 
    | (node_lst, edge_lst) -> 
        (match edge_lst with
        | [] -> true
        | (n1, n2)::rest2 -> 
          if getColor n1 coloring <> getColor n2 coloring 
          then true && (isValidColor (node_lst, rest2) coloring)
          else false
        )
let color_option (g : graph) : coloring option =
   let rec helper partial_subset rest
      = if isValidColor g partial_subset && partial_subset <> [] && rest = [] 
        then
          Some partial_subset
        else
          match rest with
          | [] -> None
          | x::xs -> match helper ((x, C 1)::partial_subset) xs with
                     | Some result -> Some result 
                     | None -> (match helper ((x, C 2)::partial_subset) xs with
                                | Some result -> Some result
                                | None -> helper ((x, C 3)::partial_subset) xs
                                )
    in 
    let (nde_lst, edge_lst) = g
    in 
    helper [] (List.rev nde_lst)

exception FoundColoring of coloring

let color_exception (g: graph) : unit =
     let rec helper partial_subset rest
      = if isValidColor g partial_subset && partial_subset <> [] && rest = [] 
        then
          raise (FoundColoring partial_subset)
        else
          match rest with
          | [] -> ()
          | x::xs -> helper ((x, C 1)::partial_subset) xs ; 
          helper ((x, C 2)::partial_subset) xs; 
          helper ((x, C 3)::partial_subset) xs
    in 
    let (nde_lst, edge_lst) = g
    in 
    helper [] (List.rev nde_lst)

let g1 =( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4)] )

let g1_coloring = [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]
