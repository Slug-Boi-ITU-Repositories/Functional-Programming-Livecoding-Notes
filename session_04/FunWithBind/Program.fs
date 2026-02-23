
let rng = System.Random()
let malicious_fun i = if rng.Next 100 > 10 then Some i else None


let add5 a = a + 5
let mul3_fault a = malicious_fun (a * 3)

let add5_on_opt =
    let x_opt = malicious_fun 10    
    match x_opt with
    | Some x -> Some(add5 x)
    | None -> None

let (>>=) opt binder = Option.bind binder opt 


let add5_on_opt_2 () =
    let x_opt = malicious_fun 10  

    x_opt 
    |> Option.bind ( 
        fun x -> mul3_fault x 
                 |> Option.bind (fun y -> Some (add5 y)))

let add5_on_opt_3 () =
    let x_opt = malicious_fun 10  

    x_opt >>= fun x -> mul3_fault x >>= fun y -> Some (add5 y)