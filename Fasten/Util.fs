namespace Fasten
module Util =

    open System
    open System.Collections.Generic

    type RandomStep =
        | Down
        | Stay
        | Up

    let randomStep (generator : Random) =
        let value = generator.NextDouble ()
        if value < 0.33 then Down
        elif value < 0.66 then Stay
        else Up

    let errorf (format : Printf.TextWriterFormat<'a>) : 'a =
        fprintf stderr format

    let errorfn (format : Printf.TextWriterFormat<'a>) : 'a =
        fprintfn stderr format

    let isPowerOfTwo (x : int64) : bool =
        x <> 0L && (x &&& (x - 1L)) = 0L

    let pair (a : 'a) (b : 'b) : 'a * 'b =
        a, b

    let zipi<'a> : IEnumerable<'a> -> seq<int * 'a> =
        Seq.mapi pair

    let randomInRange (generator : Random) (range : int) : int =
        if range = 0 then 0
        else int32 (uint32 (generator.Next ()) % uint32 range)

    let mapIndex (f : 'a -> 'a) (xs : 'a []) (i : int) =
        Array.concat [
            Array.sub xs 0 i
            [|f xs.[i]|]
            Array.sub xs (i + 1) (Array.length xs - i - 1)
        ]

    let mapRandom (generator : Random) (f : 'a -> 'a) (xs : 'a []) : 'a [] =
        if xs.Length = 0 then xs
        else randomInRange generator xs.Length |> mapIndex f xs

    let swap (a: 'T []) (i : int) (j : int) =
        let x = a.[i]
        a.[i] <- a.[j]
        a.[j] <- x

    let shuffleInPlace (generator : Random) (xs : 'T []) =
        Array.iteri (fun i _ -> swap xs i (generator.Next (i, xs.Length))) xs
