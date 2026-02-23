// For more information see https://aka.ms/fsharp-console-apps

let sc = Kattio.Scanner()

let rows = sc.NextInt()
let cols = sc.NextInt()

let secret_message: char list = List.Empty


let wrapper =
    List.init rows (fun _ -> sc.Next())
    |> List.map (fun item -> Array.filter (fun ch -> ch <> '.') (item.ToCharArray()) |>  System.String.Concat) |> System.String.Concat

printf "%s" wrapper
