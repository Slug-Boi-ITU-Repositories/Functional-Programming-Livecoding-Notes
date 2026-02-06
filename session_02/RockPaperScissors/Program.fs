open System

type Move =
    | Rock
    | Paper
    | Scissors

type Result =
    | Win
    | Loss
    | Draw    

// Determines result for player1
let game player1 player2 = 
    match player1,player2 with
    | Rock, Scissors -> Win
    | Rock, Paper -> Loss
    | Paper, Rock -> Win
    | Paper, Scissors -> Loss
    | Scissors, Paper -> Win
    | Scissors, Rock -> Loss
    | _,_ -> Draw

// Generates a random move
let randomMove(rng: Random) =
    match rng.Next(3) with
        | 0 -> Rock
        | 1 -> Paper
        | _ -> Scissors

let stringToMove(s:String) = 
    match s.Trim().ToLower() with
    | "rock" -> Rock
    | "paper" -> Paper
    | "scissors" -> Scissors
    | _ -> exit 1

let resultToString(r:Result) =
    match r with
    | Win -> "You won!"
    | Loss -> "You lose. Better luck next time!"
    | Draw -> "It's a draw" 
    
[<EntryPoint>]
let main argv =
    let sc = Kattio.Scanner()
    let rng = Random()
    
    printfn "Let's play Rock, Paper, Scissors!"
    printfn "Enter your move (rock/paper/scissors): "
    let playerMove = sc.Next() |> stringToMove
    let rndMove = randomMove(rng)
    let result = game playerMove rndMove

    printfn "Opponent played %A"  rndMove
    printfn "%s" (resultToString result)
    0