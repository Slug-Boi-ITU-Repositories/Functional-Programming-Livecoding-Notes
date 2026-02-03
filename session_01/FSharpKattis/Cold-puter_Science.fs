module Coldputer

let run () =
    let sc = Kattio.Scanner()
    let nums = sc.NextInt()

    let rec helper count =
        if count = 0 then 0
        else if sc.NextInt() < 0 then helper (count - 1) + 1
        else helper (count - 1)

    printfn "%d" (helper nums)

let run_fold () =
    let sc = Kattio.Scanner()
    let length = sc.NextInt()

    let out =
        Array.init length (fun _ -> sc.NextInt())
        |> Array.fold (fun acc elem -> if elem < 0 then acc + 1 else acc) 0

    printfn "%A" out
