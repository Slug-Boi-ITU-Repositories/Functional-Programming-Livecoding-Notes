module Exam2023
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2023 = 
 *)

(* 1: Logic *)

    type prop =  
    | TT  
    | FF  
    | And of prop * prop  
    | Or of prop * prop
    
    
    
    let p1 = And(TT, FF)  
    let p2 = Or(TT, FF)  
    let p3 = And(Or(TT, And(TT, FF)), TT)  
    let p4 = And(Or(TT, And(TT, FF)), Or(FF, And(TT, FF)))
    
(* Question 1.1: Evaluation *)

    let rec eval (p : prop) : bool  = 
        match p with
        | TT   -> true
        | FF   -> false
        | And (p1, p2) ->  
            eval p1  |> fun b1 -> eval p2 |> fun b2 ->  b1 && b2
        | Or  (p1, p2) -> 
            let b1 = eval p1
            let b2 = eval p2

            b1 || b2
    
(* Question 1.2: Negation and implication *)
    let rec negate (p : prop) : prop = 
        match p with
        | TT -> FF
        | FF -> TT
        | And (p, q) -> Or(negate p, negate q)
        | Or (p, q) -> And (negate p, negate q)
    let implies p q = Or(negate p, q)

(* Question 1.3: Bounded universal quantifiers *)
    
    // NOT TAIL RECURSIVE!
    let rec forall (f : 'a -> prop) =
        function
        | [] -> TT
        | x :: xs -> And(f x, forall f xs)
        
    (* Tail recursive:
        let rec forall (f : 'a -> prop) (lst : 'a list) =
            let rec inner (f : 'a -> prop) (_lst : 'a list) acc = 
                match _lst with
                | [] -> acc
                | x :: xs -> inner f xs (And(acc, f x))
            inner f lst TT
    *)
        


(* Question 1.4: Bounded existential quantifiers *)
    let exists (f : 'a -> prop) (lst : 'a list) = 
        lst |> List.fold (fun acc x -> Or(acc , f x)) FF

    (*
    let exists (f : 'a -> prop) (lst : 'a list) = 
        List.map f lst |> List.map eval |> List.exists id |> fun b -> if b then TT else FF 
    *)

(* Question 1.5: Bounded unique existential quantifiers *)

    let existsOne f lst = failwith "not implemented"

(*    
    let existsOne f lst = 
        match lst with
        | [] -> FF
        | x :: xs -> And()
*)

(* 2: Code Comprehension *)
 
    let rec foo xs ys =  
        match xs, ys with  
        | _       , []                  -> Some xs   
        | x :: xs', y :: ys' when x = y -> foo xs' ys'   
        | _       , _                   -> None  
          
    let rec bar xs ys =
        match foo xs ys with
        | Some zs -> bar zs ys
        | None -> match xs with
                  | [] -> []
                  | x :: xs' -> x :: (bar xs' ys)  

    let baz (a : string) (b : string) =  
        bar [for c in a -> c] [for c in b -> c] |>  
        List.fold (fun acc c -> acc + string c) ""

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo : 'a list -> 'a list -> 'a list option
       bar : 'a list -> 'a list -> 'a list
       baz : string -> string -> string


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: foo goes through the two lists if the elements are the same (at each index) and returns Some plus the remainder of 
       xs if it as long as or longer than ys. In all other cases it returns None.
       
       bar returns a list of xs excluding parts that are equal to the whole ys list. Thus it removes ys from xs.
       
       baz removes b from a if b is a substring of a and returns the resulting string.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo : removeSublistAux
       bar : removeSublist
       baz : removeSubstring
        
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `[for c in a -> c]`
    * B: `[for c in b -> c]`
    * C: `List.fold (fun acc c -> acc + string c) ""`

    Q: In the context of the baz function, i.e. assuming that `a` and `b` are strings, 
       what are the types of snippets A, B, and C and what are they -- 
       focus on what they do rather than how they do it.
    
    A: A returns 'a' as a list of chars. 
       B returns 'b' as a list of chars.
       C takes a list of chars and returns it as a string. All the snippets keep the order of the string/list.
    
    Q: Explain the use of the `|>`-operator in the baz function.

    A: the '|>' operator takes the output from the previous function (in this case the result of calling bar) and pipes 
       it into the following function as the last parameter (in this case the list for the fold function).

    *)

(* Question 2.3: No recursion *) 

    let foo2 xs ys = 
        let l = List.length xs
        let ly = List.length ys
        if l < ly
        then None
        else
            let prefix,rest = List.splitAt ly xs
            if (prefix = ys) then Some rest else None

(* Question 2.4 *)

    (*

    Q: The function `bar` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: <Your answer goes here>

    *)
(* Question 2.5 *)

    let barTail _ = failwith "not implemented"

(* 3: Collatz Conjecture *)

(* Question 3.1: Collatz sequences *)

    let collatz _ = failwith "not implemented"

(* Question 3.2: Even and odd Collatz sequence elements *)

    let evenOddCollatz _ = failwith "not implemented"

(* Question 3.3: Maximum length Collatz Sequence *)
  
    let maxCollatz _ = failwith "not implemented"

(* Question 3.4: Collecting by length *)
    let collect _ = failwith "not implemented"
    
(* Question 3.5: Parallel maximum Collatz sequence *)

    let parallelMaxCollatz _ = failwith "not implemented"

(* 4: Memory machines *)

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of expr             // Memory lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
          
    type stmnt =  
    | Assign of expr * expr      // Assign value to memory location
    | While  of expr * prog      // While loop
      
    and prog = stmnt list        // Programs are sequences of statements

    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    let (.<-.) e1 e2 = Assign (e1, e2)
    
    // Starting from memory {0, 0, 2, 0}
    let fibProg x =  
        [Num 0 .<-. Num x       // {x, 0, 2, 0}
         Num 1 .<-. Num 1       // {x, 1, 2, 0}
         Num 2 .<-. Num 0       // {x, 1, 0, 0}
         While (Lookup (Num 0), 
                [Num 0 .<-. Lookup (Num 0) .-. Num 1  
                 Num 3 .<-. Lookup (Num 1)  
                 Num 1 .<-. Lookup (Num 1) .+. Lookup (Num 2)  
                 Num 2 .<-. Lookup (Num 3)  
                ]) // after loop {0, fib (x + 1), fib x, fib x}
         ]

(* Question 4.1: Memory blocks *)

    type mem = unit (* replace this entire type with your own *)
    let emptyMem _ = failwith "not implemented"
    let lookup _ = failwith "not implemented"
    let assign _ = failwith "not implemented"

(* Question 4.2: Evaluation *)

    let evalExpr _ = failwith "not implemented"
    let evalStmnt _ = failwith "not implemented"
    let evalProg _ = failwith "not implemented"
    
(* Question 4.3: State monad *)
    type StateMonad<'a> = SM of (mem -> ('a * mem) option)  
      
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
      
    let evalSM m (SM f) = f m

    let lookup2 _ = failwith "not implemented"
    let assign2 _ = failwith "not implemented"

(* Question 4.4: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let evalExpr2 _ = failwith "not implemented"
    let evalStmnt2 _ = failwith "not implemented"
    let evalProg2 _ = failwith "not implemented"
    
(* Question 4.5: Parsing *)
    
    open JParsec.TextParser
      
    let ParseExpr, eref = createParserForwardedToRef<expr>()  
    let ParseAtom, aref = createParserForwardedToRef<expr>()  
      
    let parseExpr _ = failwith "not implemented" // Parse addition and minus
          
    let parseAtom _ = failwith "not implemented" // Parse numbers and lookups

//    Uncomment the following two lines once you finish parseExpr and parseAtom             
//    do aref := parseAtom  
//    do eref := parseExpr  
      