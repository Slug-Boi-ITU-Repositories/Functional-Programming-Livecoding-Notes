let mutable a = 15

printfn "%d" a

a <- 20

printfn "%d" a

let x = 15

let f y =
    let x = y + 1
    x * x

let result = f 2

printfn "%d" result

printfn "%d" x // x remains unchanged despite function "changing" it

let mutable x_mut = 15

let f_mut y =
    x_mut <- y + 1
    x_mut * x_mut

printfn "%d" (f_mut 2)

printfn "%d" x_mut // x has changed in global context despite only being edited in local scope of function


open System.Collections.Generic

// Mutable data when it goes wrong
let mutable l = new List<int>([ 1; 2; 4; 8 ])

let first () = l.[0]

let mal_func x =
    l.Clear()
    x + 2

let res = mal_func 2


// Passing state along
let map = Map.empty |> Map.add 2 2 |> Map.add 3 3 |> Map.add 4 5

printfn "%d" (map.Item 4)
