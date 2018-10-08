let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "

let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i =
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."

let read_file (file_name: string) : char list =
  let ic = open_in file_name
  in
  let rec read_chars ic =
    try
      let next_char = input_char ic
      in next_char :: read_chars ic
    with
      _ -> []
  in read_chars ic

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

let split f lst =
  let helper (x1,x2) x =
    if f x then (x1@[x2], []) else (x1, x2@[x])
  in
  let (t1, t2) = List.fold_left helper ([],[]) lst
  in
  t1@[t2]

let format line int =
    let charList = explode line
    in
    let splited = split (fun x -> x = '\t' || x='\n' || x= ' ') charList
    in
    let stringList = List.map implode splited
    in
    let filtered = List.filter (fun x -> x <> "") stringList
    in
    let helper (x1, totalLength, int) x =
         if (totalLength + String.length x) > int then
           (x1@["\n"]@[x], String.length x+1, int)
         else if (totalLength + String.length x)+1 = int then
          (x1@[" "]@[x], int, int)
          else                  (*length tempString + length x +1 <int*)
          (x1@[" "]@[x], (totalLength + String.length x)+1, int)
    in
    let (t1, totalLength, count) = List.fold_left helper ([], 0, int) filtered
    in
    let result = String.trim (String.concat "" t1)
    in
    result
