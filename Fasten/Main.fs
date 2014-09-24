open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Text.RegularExpressions

(* Types. *)

type Column = uint32
type DirectoryPath = string
type FilePath = string
type Fitness = float
type Length = uint32
type Line = int

type Value =
    | Regular of int64
    | PowerOfTwo of int64
    | Boolean of bool

type Command = unit -> option<string>

[<NoComparison; NoEquality>]
type CommandLineOptions = {
    buildProcedure : option<Command>
    externalProcessTimeout : int
    fastenableRegex : Regex
    fileRegex : Regex
    fitnessProcedure : option<Command>
    generations : int
    populationSize : int
    resetProcedure : option<Command>
    verbose : bool
}

type Fastener = {
    path : FilePath
    line : Line
    original : Value
    value : Value
}

let stringOfValue (value : Value) : string =
    match value with
    | Regular i -> sprintf "%d" i
    | PowerOfTwo i -> sprintf "%d" i
    | Boolean b -> if b then "1" else "0"

(* FIXME: Is there no way to override “ToString” for record types? *)
let stringOfFastener (fastener : Fastener) : string =
    if fastener.value = fastener.original then ""
    else
        sprintf
            "%s:%d: change %s to %s"
            fastener.path
            fastener.line
            (stringOfValue fastener.original)
            (stringOfValue fastener.value)

[<NoComparison>]
type File = {
    path : FilePath
    lines : (int * string) []
    fasteners : Fastener []
}

type Individual = File []

type Population = Individual []

[<NoComparison>]
type ExerciseResult = {
    individual : Individual
    fitness : Fitness
}

type RandomStep =
    | Down
    | Stay
    | Up

(* Things that should exist. *)

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

(* Error reporting utilities. *)

let reportUsage () : unit =
    errorf
        "Usage:\n\
        \n\
        \tfasten [<options>] <directories>\n\
        \n\
        Required:\n\
        \n\
        \t--build <command>\n\
        \t\tThe command that builds the source tree (e.g., 'make').\n\
        \n\
        \t--fitness <command>\n\
        \t\tThe command that computes fitness (e.g. 'bin/run-benchmark').\n\
        \n\
        \t--reset <command>\n\
        \t\tThe command that resets the source tree (e.g. 'git checkout .').\n\
        \n\
        Optional:\n\
        \n\
        \t--files <regex>\n\
        \t\tRegular expression matching file names to search.\n\
        \n\
        \t--generations <count>\n\
        \t\tNumber of generations to run (default 20).\n\
        \n\
        \t--population <size>\n\
        \t\tSize of a population (default 20).\n"
    exit 1

let reportError (``exception`` : 'a) (message : string) : 'b =
    errorfn "Error: %s\n%s" (``exception``.ToString ()) message
    exit 1

let reportDirectoryNotFound (name : DirectoryPath) : 'a =
    errorfn "Directory not found: %s" name
    exit 1

let reportFailedCommand (command : string) (errors : option<string>) : 'a =
    errorfn "Command failed: %s" command
    match errors with
    | Some errors -> errorfn "%s" errors
    | None -> ()
    exit 1

let reportInvalidFitnessOutput (fitnessOutput : string) =
    errorfn "Invalid fitness function output: '%s'" fitnessOutput
    exit 1

let reportInvalidFlag (flag : string) (expected : string) : 'a =
    errorfn "Invalid flag %s; expected %s." flag expected
    exit 1

(* Running external processes. *)

let runCommand (options : CommandLineOptions) (command : string) ()
    : option<string> =
    try
        use mutable ``process`` = new Process ()
        (* FIXME This regex is brittle, but I didn’t want to add separate
            command-line options for process arguments. *)
        let ``match`` = Regex.Match (command, "^(\\S+)(.*)$")
        let executable = ``match``.Groups.[1].Value
        let arguments = ``match``.Groups.[2].Value
        ``process``.StartInfo.FileName <- executable
        ``process``.StartInfo.Arguments <- arguments
        ``process``.StartInfo.UseShellExecute <- false
        ``process``.StartInfo.RedirectStandardOutput <- true
        ``process``.StartInfo.RedirectStandardError <- true
        ``process``.StartInfo.WorkingDirectory <-
            Directory.GetCurrentDirectory ()
        let output = new System.Text.StringBuilder ()
        let error = new System.Text.StringBuilder ()
        if ``process``.Start () then
            ``process``.OutputDataReceived.Add
                (fun args -> output.Append(args.Data) |> ignore)
            ``process``.ErrorDataReceived.Add
                (fun args -> error.Append(args.Data) |> ignore)
            ``process``.BeginErrorReadLine ()
            ``process``.BeginOutputReadLine ()
            if ``process``.WaitForExit options.externalProcessTimeout then
                printfn "Output: %s" (output.ToString ())
                printfn "Errors: %s" (error.ToString ())
                if ``process``.ExitCode = 0 then
                    Some (output.ToString ())
                else
                    printfn
                        "Process failed with exit code %d. Output:\n%s\nErrors:\n%s"
                        ``process``.ExitCode
                        (output.ToString ())
                        (error.ToString ())
                    None
            else
                printfn "Process timed out."
                if not ``process``.HasExited then
                    ``process``.Kill ()
                printfn "Output: %s" (output.ToString ())
                printfn "Errors: %s" (error.ToString ())
                (* Individuals that take too long to exercise are unfit! *)
                None
        else reportFailedCommand command None
    with e -> reportFailedCommand command (Some (e.ToString ()))

(* Command-line options. *)

let defaultOptions : CommandLineOptions = {
    buildProcedure = None
    externalProcessTimeout = 60 * 1000
    fastenableRegex = new Regex ("\\d+(?=\\s*/\\*\\s*(INT|POW|BOOL)\\s+FASTENABLE\\s*\\*/)")
    fileRegex = new Regex ("\\.c|\\.h")
    fitnessProcedure = None
    generations = 20
    populationSize = 20
    resetProcedure = None
    verbose = false
}

let parseCommandLineOptions
    : string list -> CommandLineOptions * string list =
    let rec
        go =
            fun ((options, unused) as acc) ->
                let proceed x y = go (x, unused) y in function
                | "--build" :: command :: rest ->
                    proceed
                        { options with buildProcedure = Some (runCommand options command) }
                        rest
                | "--build" :: [] ->
                    reportInvalidFlag "--build" "<command>"
                | "--files" :: pattern :: rest ->
                    proceed
                        { options with fileRegex = new Regex (pattern) }
                        rest
                | "--files" :: [] -> reportInvalidFlag "--files" "<regex>"
                | "--fitness" :: command :: rest ->
                    proceed
                        { options with fitnessProcedure = Some (runCommand options command) }
                        rest
                | "--fitness" :: [] ->
                    reportInvalidFlag "--fitness" "<command>"
                | "--generations" :: size :: rest ->
                    proceed
                        { options with generations = System.Int32.Parse size }
                        rest
                | "--generations" :: [] ->
                    reportInvalidFlag "--generations" "<count>"
                | "--population" :: size :: rest ->
                    proceed
                        { options with populationSize = System.Int32.Parse size }
                        rest
                | "--population" :: [] ->
                    reportInvalidFlag "--population" "<size>"
                | "--reset" :: command :: rest ->
                    proceed
                        { options with resetProcedure = Some (runCommand options command) }
                        rest
                | "--reset" :: [] ->
                    reportInvalidFlag "--reset" "<command>"
                | "--timeout" :: milliseconds :: rest ->
                    proceed
                        { options with externalProcessTimeout = System.Int32.Parse milliseconds }
                        rest
                | "--timeout" :: [] ->
                    reportInvalidFlag "--timeout" "<milliseconds>"
                | "--verbose" :: rest ->
                    proceed
                        { options with verbose = true }
                        rest
                | x :: rest ->
                    go (options, x :: unused) rest
                | [] -> acc
    go (defaultOptions, [])

(* Reading. *)

let readFile
    (options : CommandLineOptions) (file : FilePath) : option<File> =
    (* This is eager ('ReadAllLines' rather than 'ReadLines') in order to avoid
        too many open files in large directory trees due to lazy I/O. *)
    let lines = System.IO.File.ReadAllLines file |> zipi
    let fastenable (index, text) =
        let ``match`` = options.fastenableRegex.Match text
        if ``match``.Success && ``match``.Groups.Count >= 1 then
            Some (index, ``match``.Groups.[0].Value, ``match``.Groups.[1].Value)
        else None
    let groups = Seq.choose fastenable lines
    printfn "All groups: %A" groups
    let makeValue (t : string) (i : int64) =
        match t with
        | "INT" -> Regular i
        | "POW" -> PowerOfTwo i
        | "BOOL" -> Boolean (i <> 0L)
        | _ -> raise (Exception "Illegal type")
    let fastenerOfGroup (line, group : string, ``type`` : string) =
        let value = makeValue ``type`` (System.Int64.Parse group)
        {
            path = file
            line = line
            original = value
            value = value
        }
    let fasteners = Seq.map fastenerOfGroup groups
    if Seq.isEmpty fasteners then None
    else
        printfn "File %s contains %d fasteners." file (Seq.length fasteners)
        Some {
            path = file
            lines = Array.ofSeq lines
            fasteners = Array.ofSeq fasteners
        }

let readDirectory
    (options : CommandLineOptions) (directory : DirectoryPath) : seq<File> =
    let files =
        try Directory.GetFiles (directory, "*", SearchOption.AllDirectories)
        with
        | :? System.IO.DirectoryNotFoundException ->
            reportDirectoryNotFound directory
        | e -> reportError e ("Directory: " + directory)
    files
        |> Seq.filter options.fileRegex.IsMatch
        |> Seq.choose (readFile options)

(* Genetic mutation. *)

let mutateValue (generator : Random) (value : Value) : Value =
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

let mutateFastener
    (generator : Random) (fastener : Fastener) : Fastener =
    { fastener with value = mutateValue generator fastener.value }

let mutateFile (generator : Random) (file : File) : File =
    { file with
        fasteners = mapRandom generator
            (mutateFastener generator) file.fasteners }

let mutateIndividual (generator : Random) =
    mapRandom generator (mutateFile generator)

(* Fitness testing. *)

let exercisePopulation
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
                match fitness with
                | Some fitness ->
                    Some { individual = individual; fitness = fitness }
                | None -> None end
            population
        |> Array.choose id

(* Generating and running generations. *)

let generatePopulation
    (generator : Random) (size : int) (individual : Individual)
    : Population =
    Seq.init size
        (fun _ -> mutateIndividual generator individual)
        |> Array.ofSeq

let rec runGeneration
    (generator : Random)
    (generations : int)
    (initial : Individual)
    (options : CommandLineOptions)
    (population : Population)
    : Population =
    printfn "%d generations remain." generations
    let results =
        exercisePopulation options population
            |> Array.sortBy (fun result -> -result.fitness)
    let oneHalf = results.Length / 2
    let fittest = Seq.take oneHalf (Array.toSeq results) |> Seq.toArray
    Array.Reverse fittest
    let breed (generator : Random) (a : Individual) (b : Individual)
        : Individual =
        let mergeFiles x y =
            let i = randomInRange generator x.fasteners.Length
            { x with
                fasteners =
                    Seq.append
                        (Seq.take i (Array.toSeq x.fasteners))
                        (Seq.skip i (Array.toSeq y.fasteners))
                    |> Seq.toArray }
        (* Assumes the two individuals have their files in the same order. *)
        Array.map2 mergeFiles a b
    let cross (generator : Random) (group : ExerciseResult [])
        : seq<Individual> =
        let sums =
            Array.sub
                (Array.scan (fun x result -> x + result.fitness) 0.0 group)
                1 group.Length
        let weightedRandomIndividual () =
            let position = generator.NextDouble () * sums.[sums.Length - 1]
            match Array.tryFindIndex (fun x -> position <= x) sums with
            | Some index -> group.[index].individual
            | None -> group.[group.Length - 1].individual
        Seq.init group.Length
            (fun _ ->
                breed
                    generator
                    (weightedRandomIndividual ())
                    (weightedRandomIndividual ()))
    let crossed = cross generator fittest
    let mutants =
        Seq.map
            (fun result -> mutateIndividual generator result.individual)
            fittest
    let newPopulation = Seq.append mutants crossed |> Seq.toArray
    if generations < 1 then newPopulation
    else
        runGeneration generator (generations - 1) initial options newPopulation

(* Entry point. *)

[<EntryPoint>]
let main (argv : string []) : int =
    let options, directories = Array.toList argv |> parseCommandLineOptions
    if List.isEmpty directories
        || options.buildProcedure.IsNone
        || options.fitnessProcedure.IsNone
        || options.resetProcedure.IsNone then
        reportUsage ()
    printfn "Loading files."
    let initialIndividual =
        List.map (readDirectory options) directories
            |> Seq.concat |> Array.ofSeq
    let generator = new Random ()
    (* Since we only have one example individual—the initial source tree—we
        generate an initial population by mutating the original, which is
        assumed to be reasonably fit already. *)
    printfn "Computing initial population."
    let initialPopulation =
        generatePopulation
            generator
            options.populationSize
            initialIndividual
    let finalPopulation =
        runGeneration
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
