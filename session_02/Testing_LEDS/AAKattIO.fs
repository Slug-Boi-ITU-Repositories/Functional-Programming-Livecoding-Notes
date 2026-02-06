namespace Kattio
open System
open System.IO

exception NoMoreTokensException

type Tokenizer(inStream : Stream) =

    let bs = new BufferedStream(inStream);
    let reader = new StreamReader(bs)

    let mutable tokens = Array.empty
    let mutable pos = 0
    
    let rec peek =
        function
        | () when pos < 0 -> None
        | () when pos < Array.length tokens -> 
            let s = tokens.[pos]
            if s = "" then
                pos <- pos + 1
                peek ()
            else
                Some (tokens.[pos])
        | () -> 
            let line = reader.ReadLine()
            if line = null then
                pos <- -1
                None
            else 
                tokens <- line.Split (' ')
                pos <- 0
                peek ()

    new() = Tokenizer(Console.OpenStandardInput())

    member this.hasNext() = peek() <> None

    member this.Next() =
        match peek() with
        | None   -> raise NoMoreTokensException
        | Some s -> pos <- pos + 1; s 


type Scanner(inStream : Stream) =
    inherit Tokenizer(inStream)   

    new() = Scanner(Console.OpenStandardInput())

    member this.NextInt() = this.Next() |> int
    member this.NextLong() = this.Next() |> int64
    member this.NextFloat() = this.Next() |> float
    member this.NextDouble() = this.Next() |> double
    member this.NextChar() = this.Next() |> char

type BufferedStdoutWriter() =
    inherit StreamWriter(new BufferedStream(Console.OpenStandardOutput()))

