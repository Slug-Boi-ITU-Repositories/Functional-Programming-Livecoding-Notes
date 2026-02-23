module Autori
open System

let run() =
    let name = 
        Kattio.Scanner().Next() 
        |> Seq.toList
        |> List.filter (fun c -> System.Char.IsUpper(c))
        |> System.String.Concat

    printfn "%s" name