module Bitte

let run () = 
    let sc = Kattio.Scanner()
    let str = sc.Next()
    printfn "%c" str[0]
