module Sauron

let rec count_lines lst =
    match lst with
    | [] -> 0
    | '|' :: xs -> 1 + count_lines xs
    | _ -> 0


let run () = 
    let lst = Kattio.Scanner().Next().ToCharArray() |> Array.toList
    let leading = count_lines lst
    let trailing = List.rev lst |> count_lines
    match leading = trailing with
    | true  -> printfn "correct"
    | false -> printfn "fix"



let rec check_pairs pairs = 
    match pairs with
    | ('|', '|') :: xs -> check_pairs xs
    | ('(', ')') :: _ -> "correct"
    | _ -> "fix"

let run_zip () = 
    Kattio.Scanner().Next().ToCharArray() 
    |> Array.toList 
    |> fun lst -> List.zip lst (List.rev lst) 
    |> check_pairs
    |> printfn "%s"
