namespace Fasten
module Exercise =

    open System.IO

    open Fasten.Options
    open Fasten.Types

    let population
        (options : CommandLineOptions) (population : Population)
        : ExerciseResult [] =
        printfn "Exercising population."
        let writeFile file =
            let writer = new StreamWriter (file.path)
            for line, text in file.lines do
                let found =
                    Array.tryFind
                        (fun fastener -> fastener.line = line)
                        file.fasteners
                let text =
                    match found with
                    | Some found ->
                        options.fastenableRegex.Replace
                            (text, (stringOfValue found.value))
                    | None -> text
                writer.WriteLine text
            writer.Flush ()
            writer.Close ()
        let writeIndividual =
            Array.iter writeFile
        let exercise individual =
            printfn "Resetting tree."
            options.resetProcedure.Value () |> ignore
            printfn "Writing individual."
            writeIndividual individual
            printfn "Building."
            let buildStatus = options.buildProcedure.Value ()
            match buildStatus with
            | Some status ->
                printfn "Testing fitness."
                let fitnessOutput = options.fitnessProcedure.Value ()
                match fitnessOutput with
                | Some output ->
                    try
                        let fitness = 1.0 / System.Double.Parse output
                        printfn "Calculated fitness: %f." fitness
                        Some fitness
                    with
                    | :? System.FormatException ->
                        printfn
                            "Individual produced an invalid fitness result: '%s'"
                            output
                        None
                    | e ->
                        printfn
                            "Individual did not produce a fitness result:\n%s"
                            (e.ToString ())
                        None
                | None ->
                    printfn "Individual died during exercise. :("
                    None
            | None -> None
        Array.map exercise population
            |> Array.map2
                begin fun individual fitness ->
                    let fitness =
                        match fitness with
                        | Some fitness -> fitness
                        | None -> 0.0
                    Some { individual = individual; fitness = fitness }
                end
                population
            |> Array.choose id
