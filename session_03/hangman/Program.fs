// For more information see https://aka.ms/fsharp-console-apps
open System.IO

// let word = [ 'C'; 'A'; 'T' ] |> List.map System.Char.ToUpper

let loadDictionary (path: string) : string list =
    File.ReadAllLines path |> Array.toList |> List.map (fun s -> s.ToUpper())

let word =
    loadDictionary "./words.txt"
    |> (fun lst -> List.item (System.Random().Next(lst.Length)) lst)
    |> Seq.toList

let prettyPrint (set: char list) =
    List.map (fun ch -> printf "%c " ch) set |> ignore
    printfn ""

let GoodPrettyPrint (set: char list) =
    System.String.Join(" ", Array.ofList(set))

let check guessed =
    List.map
        (fun (ch: char) ->
            match Set.contains ch guessed with
            | true -> ch
            | _ -> '_')
        word

let validate guessed =
    List.forall (fun ch -> Set.contains ch guessed) word

let sc = Kattio.Scanner()

let rec run lives (guessed: char Set) =
    if lives = 0 then "You died lol" else

    let new_guessed = sc.NextChar() |> System.Char.ToUpper |> guessed.Add
    check new_guessed |> prettyPrint


    match validate new_guessed with
    | true -> "Congratulations you won"
    | _ ->
        printfn "You current guesses are:"
        prettyPrint (Set.toList new_guessed)
        run (lives - 1) new_guessed

printfn "Welcome to Hangman where no men will be hanged (PG 7)"
check Set.empty |> prettyPrint

printfn "%s" (run 10000 Set.empty)
