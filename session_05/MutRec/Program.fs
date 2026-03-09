

let rec sumEvenPositions list =
    match list with
    | [] -> 0
    | x :: xs -> x + sumOddPositions xs

and sumOddPositions list =
    match list with
    | [] -> 0
    | _ :: xs -> sumEvenPositions xs




let rec playerATurn n =
    if n <= 0 then
        printfn "Game over"
    else
        printfn "Player A moves"
        playerBTurn (n - 1)

and playerBTurn n =
    if n <= 0 then
        printfn "Game over"
    else
        printfn "Player B moves"
        playerATurn (n - 1)

