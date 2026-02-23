module Interval

let run () = 
    Kattio.Scanner () 
    |> fun sc -> Array.init (sc.NextInt ()) (fun _ -> sc.NextInt (), sc.NextInt ()) 
    |> Array.toList
    |> List.sortBy snd
    |> List.fold (
        fun acc t -> 
            match fst t >= snd acc with
            | true  -> (fst acc + 1, snd t)
            | false -> acc
        ) (0,0)
    |> fst
    |> printfn "%d"
