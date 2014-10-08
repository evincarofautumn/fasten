namespace Fasten
module Mutate =

    open System

    open Fasten.Types
    open Fasten.Util

    let value (generator : Random) (value : Value) : Value =
        let step = randomStep generator
        let nonZeroOrDefault i d =
            if i = 0L then
                d
            else
                i
        match value with
        | Regular i ->
            match step with
            | Down -> Regular (i - 1L)
            | Stay -> value
            | Up -> Regular (i + 1L)
        | PowerOfTwo i ->
            match step with
            | Down -> PowerOfTwo (nonZeroOrDefault (i >>> 1) i)
            | Stay -> value
            | Up -> PowerOfTwo (nonZeroOrDefault (i <<< 1) i)
        | Boolean b ->
            match step with
            | Down | Up -> Boolean (not b)
            | Stay -> value

    let fastener
        (generator : Random) (fastener : Fastener) : Fastener =
        { fastener with value = value generator fastener.value }

    let file (generator : Random) (file : File) : File =
        { file with
            fasteners = mapRandom generator
                (fastener generator) file.fasteners }

    let individual (generator : Random) =
        mapRandom generator (file generator)
