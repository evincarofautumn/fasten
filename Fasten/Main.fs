module Fasten.Main

open System

open Fasten.Types

[<EntryPoint>]
let main (argv : string []) : int =
    let options, directories = Array.toList argv |> Options.parse
    if List.isEmpty directories
        || options.buildProcedure.IsNone
        || options.fitnessProcedure.IsNone
        || options.resetProcedure.IsNone then
        Report.usage ()
    printfn "Loading files."
    let initialIndividual =
        List.map (Read.directory options) directories
            |> Seq.concat |> Array.ofSeq
    let generator = new Random ()
    (* We generate an initial population by mutating each fastener
       in the original individual for each new individual. *)
    printfn "Computing initial population."
    let initialPopulation =
        Generation.make
            generator
            options.populationSize
            initialIndividual
    let finalPopulation =
        Generation.run
            generator
            options.generations
            initialIndividual
            options
            initialPopulation
    finalPopulation
        |> Array.map
            (Array.map
                (fun file -> Array.map stringOfFastener file.fasteners))
        |> printfn "%A"
    0
