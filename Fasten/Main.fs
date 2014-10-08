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
    (* Since we only have one example individual—the initial source tree—we
        generate an initial population by mutating the original, which is
        assumed to be reasonably fit already. *)
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
