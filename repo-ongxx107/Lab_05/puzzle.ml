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

let (d1: string) = "../../public-class-repo/Homework/Files/words-small.txt"
let (d2: string) = "../../public-class-repo/Homework/Files/words-google-10000.txt"

let answers (file: string) : string list= (*add the input and output type*)
    let charList = read_file file
    in
    let stringList = split (fun x -> x = '\n' || x = ' ') charList
    in
    let filteredEmptyList = List.filter (fun x -> x<>[]) stringList
    in
    let finalStringList = List.map (fun x -> implode x) filteredEmptyList
    in
    let w6 = List.filter (fun x -> String.length x = 6) finalStringList
    in
    let w4 = List.filter (fun x -> String.length x = 4) finalStringList
    in
    let finalResult = List.filter (fun x -> List.mem (String.sub x 1 4) (w4)) w6
    in
    finalResult

let pretty_answers (stringList: string list): (string*string) list =
    (*add the input and output type*)
    let result = List.map (fun x -> (String.sub x 1 4, x)) stringList
    in result
