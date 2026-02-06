// For more information see https://aka.ms/fsharp-console-apps
[<EntryPoint>]
let main argv =
    let sc = Kattio.Scanner()

    let count = sc.NextInt()

    let rec helper counter  =
        match counter with
        | 0 -> System.UInt32.MaxValue
        | v ->
            (fun (time, status) ->
                match status with
                | 0 -> 
                    let res = helper (v - 1) 
                    if time < res then time else res
                | _ -> helper (v - 1) 

            ) (
                sc.Next() |> uint32,
                sc.NextInt()
            )

    let result = helper (count) 

    match result with
    | r when r = System.UInt32.MaxValue -> printf "-1"
    | r -> printf "%d" r

    0
