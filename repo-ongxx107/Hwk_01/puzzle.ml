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

let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"

let answers (file: string) : string list=
    let charList = read_file file
    in
    let stringList = split (fun x -> x = '\n' || x = ' ') charList
    in
    let result = List.filter (fun x -> x<>[]) stringList
    in
    let final = List.map (fun x -> implode x) result
    in
    let w6 = List.filter (fun x -> String.length x = 6) final
    in
    let w4 = List.filter (fun x -> String.length x = 4) final
    in
    let finalResult = List.filter (fun x -> List.mem (String.sub x 1 4) (w4)) w6
    in
    finalResult
    
let pretty_answers stringList =
    let result = List.map (fun x -> (String.sub x 1 4, x)) stringList
    in result



