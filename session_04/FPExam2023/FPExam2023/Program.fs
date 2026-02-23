open System
open Exam2023

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    
    printfn "%A" (eval TT)
    printfn "%A" (eval FF)
    printfn "%A" (eval p1)
    printfn "%A" (eval p4)

    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    printfn "%A" (negate TT)
    printfn "%A" (negate FF)
    printfn "%A" (negate p4)
    
    
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ1 ()
    testQ2 ()
    0 // return an integer exit code
