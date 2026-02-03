open System

let sc = Kattio.Scanner()

let n = new Random () |> fun r -> 1 + r.Next(100)

let rec guess () = 
    let i = sc.NextInt()
    match i with
    | i when i = n -> 
        printfn "You guessed correctly!"
        1
    | i when i < n ->
        printfn "Your guess is too low!"
        1 + guess()
    | _ -> 
        printfn "Your guess is too high!"
        1 + guess()

printfn "Welcome to the Guessing game!"
printfn "I have a number between 1 and 100"
printfn "Get guessing!"
printfn "You used %d guesses" (guess ())