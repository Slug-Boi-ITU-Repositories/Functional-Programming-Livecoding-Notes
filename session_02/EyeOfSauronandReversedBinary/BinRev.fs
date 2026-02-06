module ReverseBinary

let rec frombitstrtoint (lst : char list) =
    match lst with
    | [] -> 0
    | '0' :: xs -> frombitstrtoint xs
    | c :: xs -> frombitstrtoint xs + (System.Math.Pow (2, List.length xs) |> int)

let rec frominttoreversedbitstr i =
    match i with
    | 0 -> []
    | 1 -> ['1']
    | n when n%2 = 1 -> '1' :: frominttoreversedbitstr ((n-1) / 2)
    | n -> '0' :: frominttoreversedbitstr (n/2)

let result_function = frominttoreversedbitstr >> frombitstrtoint

let run () = 
    Kattio.Scanner().NextInt ()
    |> result_function
    |> printfn "%d"

