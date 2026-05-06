module Exam2024

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find when switching back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2024 = 
 *)


(* 1: Transactions *)

    type transactions =
        | Empty
        | Pay     of string * int * transactions
        | Receive of string * int * transactions
        
    let rec balance (trs : transactions) : int = 
        match trs with
        | Empty -> 0
        | Pay (_, amount, tail) -> balance tail - amount
        | Receive (_, amount, tail) -> balance tail + amount
        
    let balanceAcc (trs : transactions) : int = 
        let rec inner (trs : transactions)  (acc : int) : int = 
            match trs with
            | Empty -> acc
            | Pay (_, amount, tail) -> inner tail (acc - amount)
            | Receive (_, amount, tail) -> inner tail (acc + amount)

        inner trs 0

    let rec participants (trs : transactions) : Set<string>*Set<string> = 
        match trs with
        | Empty -> Set.empty, Set.empty
        | Pay (name, _, tail) -> participants tail |> fun (set1, set2) -> (set1.Add name, set2)
        | Receive (name, _, tail) -> participants tail |> fun (set1, set2) -> (set1, set2.Add name)
    
    let rec balanceFold (payFolder : ('a -> string -> int -> 'a)) (receiveFolder : ('a -> string -> int -> 'a)) (acc : 'a) (trs : transactions) : 'a = 
        match trs with
        | Empty -> acc
        | Pay (name, amount, tail) -> balanceFold payFolder receiveFolder (payFolder acc name amount) tail
        | Receive (name, amount, tail) -> balanceFold payFolder receiveFolder (receiveFolder acc name amount) tail
    
    let collect (trs : transactions) : Map<string, int> =
        balanceFold 
            (
                fun acc name amount -> 
                let balance = acc.TryFind name |> Option.defaultValue 0
                acc.Add (name, balance - amount)
            ) (
                fun acc name amount -> 
                let balance = acc.TryFind name |> Option.defaultValue 0
                acc.Add (name, balance + amount)
            ) Map.empty trs
    
    
(* 2: Code Comprehension *)
        
    let foo (x : char) = x |> int |> fun y -> y - (int '0')
    
    let bar (x : string) = [for c in x -> c]
            
    let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo : char -> int
       bar : string -> list<char>
       baz : list<int> -> int

    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: foo takes a char and returns the corresponding integer value.
       bar takes a string and returns it as a char array
       baz takes a list of integers and returns it as an integer with the digits reversed
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo : charToInt
       bar : stringToArray
       baz : intsToReversedInt
    
    Q: The function foo only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: It makes sense to have the constraint that inputs to foo must be a character representing a base-10 digit ('0'–'9')
    
    Q: The function baz only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A:  All integers in the input should be non-negative     *)
    
(* Question 2.2 *)
    
    let stringToInt = bar >> List.map foo >> List.rev >> baz

(* Question 2.3 *)
    
    let baz2 xs = List.foldBack(fun x acc -> x + 10 * acc) xs 0
    
    let baz2Alt xs = List.fold(fun acc  a ->  a + 10 * acc) 0 (List.rev xs)
    
(* Question 2.4 *)

    (*

    Q: The function `bar` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: baz [1; 2; 3] -->
       1 + 10 * baz [2; 3] -->
       1 + 10 * (2 + 10 * baz [3]) -->
       1 + 10 * (2 + 10 * (3 + 10 * baz []))
       1 + 10 * (2 + 10 * (3 + 10 * 0))
       1 + 10 * (2 + 10 * 3)
       1 + 10 * (2 + 30)
       1 + 10 * 32
       1 + 320
       321

       baz is not tail recursive as we have 1 + 10 * baz [2; 3] for instance. 
       Until the recursive call is evaluated there is no tail to multiply 10 with and add to 1, instead the unresolved equation is 
       stored on the stack, which can eventually overflow if the lists are big enough.

    *)
    
(* Question 2.5 *)

    let bazTail xs = 
        let rec aux xs c =
            match xs with
            | [] -> c 0
            | x::xs -> aux xs (fun r -> c(x + 10 * r))
        aux xs id 
        
(* 3: Caesar Ciphers *)

(* Question 3.1 *)

    let rec rec_wrapper cha = 
        if int cha > int 'z' 
        then char (int cha - 26) 
            |> rec_wrapper else if int cha < int 'a' 
            then char (int cha + 26) |> rec_wrapper else cha

    let encrypt_char (offset: int) c = 
        match c with
        | ' ' -> " "
        | c -> char ((int c) + offset) |> rec_wrapper |> string

    let encrypt str offset = String.collect (encrypt_char offset) str
    
(* Question 3.2 *)
    let decrypt str offset = encrypt str (offset * -1)
    
(* Question 3.3 *)
    let rec rec_decode plainText encoded key =
        match key with
        | 26 -> None
        | n -> if encrypt plainText n = encoded 
                then Some n 
                else rec_decode plainText encoded (key + 1)

    let decode plainText encoded = rec_decode plainText encoded 0
    
    let rec build_str lst = 
        match lst with
        | [] -> ""
        | x :: xs -> build_str xs + (" " + x)
(* Question 3.4 *)
    let parEncrypt (str: string) (offset: int) =
     str.Split ' ' |> 
        (Array.fold (fun acc str -> async { return encrypt str offset  } :: acc ) []) 
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> Array.toList 
        |> build_str
    
(* Question 3.5 *)
        
    open JParsec.TextParser

    let parseEncrypt _ = failwith "not imlpemented"

(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type letterbox = unit // Replace with your type
    
    let empty _ = failwith "not imlpemented"

(* Question 4.2 *)

    let post _ = failwith "not imlpemented"
    
    let read _ = failwith "not imlpemented"

    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (letterbox -> ('a * letterbox) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM (SM f) = f (empty ())
    
    let post2 _ = failwith "not implemented"
    let read2 _ = failwith "not implemented"

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    type MType =
        | Post of string * string
        | Read of string
    type log = MType list
    
    let trace _ = failwith "not implemented"
