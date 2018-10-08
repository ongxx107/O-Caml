let (p1: string) = "Hello world!\n\n How are you today? \t\t I hope all is well. "

let explode (s: string) : char list =
(*This function converts the string into char list*)
  let l = String.length s
  in
  let rec f i =
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."

let read_file (file_name: string) : char list =
(*This function read the file and output a char list*)
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
(*This function converts char list into string*)
  String.concat "" (List.map  (String.make 1) cs)

let split (f: 'a -> bool) (lst: 'a list) : 'a list list = (*add the input and output type*)
(*This functions uses the function argument and splits up the list into sublists*)
    let accum = ([],[])
    in
    let helper (result1,result2) currentElement =
      if f currentElement then (result1@[result2], [])
      else (result1, result2@[currentElement])
    in
    let (result1, result2) = List.fold_left helper accum lst
    in
    result1@[result2]

let format (myString: string) (myInt: int) : string=
(*This function takes a string and format it as a paragraph according to the integer width*)
    let charList = explode myString
    in
    let splitedCharList = split (fun x -> x = '\t' || x='\n' || x= ' ') charList
    in
    let stringList = List.map implode splitedCharList
    in
    let filteredEmptyStrings = List.filter (fun x -> x <> "") stringList
    in
    let helper (x1, totalLength, int) x =
         if (totalLength + String.length x) > int then
           (x1@["\n"]@[x], String.length x+1, int)
         else if (totalLength + String.length x)+1 = int then
          (x1@[" "]@[x], int, int)
          else                  (*length tempString + length x +1 <int*)
          (x1@[" "]@[x], (totalLength + String.length x)+1, int)
    in
    let (result, totalLength, count) = List.fold_left helper ([], 0, myInt) filteredEmptyStrings
    in
    let finalResult = String.trim (String.concat "" result)
    in
    finalResult
