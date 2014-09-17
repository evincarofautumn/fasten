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
type Value = int64

type Command = unit -> option<string>

[<NoComparison>]
[<NoEquality>]
type CommandLineOptions = {
    buildProcedure : option<Command>;
    fastenableRegex : Regex;
    fileRegex : Regex;
    fitnessProcedure : option<Command>;
    populationSize : int;
    resetProcedure : option<Command>;
}

type Fastener = {
    path : FilePath;
    line : Line;
    original : Value;
    value : Value;
}

(* FIXME: Is there no way to override “ToString” for record types? *)
let stringOfFastener (fastener : Fastener) : string =
    if fastener.value = fastener.original then ""
    else
        sprintf
            "%s:%d: change %d to %d"
            fastener.path fastener.line fastener.original fastener.value

[<NoComparison>]
type File = {
    path : FilePath;
    lines : (int * string) [];
    fasteners : Fastener [];
}

type Individual = File []

type Population = Individual []

[<NoComparison>]
type ExerciseResult = {
    individual : Individual;
    fitness : Fitness;
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

let errorfn (format : Printf.TextWriterFormat<'a>) : 'a =
    fprintfn stderr format

let isPowerOfTwo (x : int64) : bool =
    x <> 0L && (x &&& (x - 1L)) = 0L

let pair (a : 'a) (b : 'b) : 'a * 'b =
    a, b

let zipi<'a> : IEnumerable<'a> -> seq<int * 'a> =
    Seq.mapi pair

let randomInRange (generator : Random) (range : int) : int =
    int32 (uint32 (generator.Next ()) % uint32 range)

let mapIndex (f : 'a -> 'a) (xs : 'a []) (i : int) =
    Array.concat
        [ Array.sub xs 0 (max 0 (i - 1))
        ; [|f (xs.[i])|]
        ; Array.sub xs (i + 1) (max 0 (xs.Length - i - 1))
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
    errorfn
        "Usage: fasten [--files <regex>] --build <command> --fitness <command> --reset <command> <directory>..."
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
    errorfn  "Invalid flag %s; expected %s" flag expected
    exit 1

(* Running external processes. *)

let runCommand (command : string) () : option<string> =
    try
        let mutable ``process`` = new Process ()
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
            (* FIXME: Put with other configuration. *)
            let timeout = 60 * 1000
            if ``process``.WaitForExit timeout then
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
    buildProcedure = None;
    fastenableRegex = new Regex ("\\d+(?=\\s*/\\*\\s*FASTENABLE\\s*\\*/)");
    fileRegex = new Regex ("\\.c|\\.h");
    fitnessProcedure = None;
    populationSize = 20;
    resetProcedure = None;
}

let parseCommandLineOptions
    : string list -> CommandLineOptions * string list =
    let rec
        go = fun ((options, unused) as acc) -> function
            | "--build" :: command :: rest ->
                go
                    ( { options
                        with buildProcedure = Some (runCommand command) }
                    , unused
                    )
                    rest
            | "--build" :: [] ->
                reportInvalidFlag "--build" "<command>"
            | "--files" :: pattern :: rest ->
                go
                    ({ options with fileRegex = new Regex (pattern) }, unused)
                    rest
            | "--files" :: [] -> reportInvalidFlag "--files" "<regex>"
            | "--fitness" :: command :: rest ->
                go
                    ( { options
                        with fitnessProcedure = Some (runCommand command) }
                    , unused
                    )
                    rest
            | "--fitness" :: [] ->
                reportInvalidFlag "--fitness" "<command>"
            | "--reset" :: command :: rest ->
                go
                    ( { options
                        with resetProcedure = Some (runCommand command) }
                    , unused
                    )
                    rest
            | "--reset" :: [] ->
                reportInvalidFlag "--reset" "<command>"
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
            Some (index, ``match``.Groups.[0])
        else None
    let groups = Seq.choose fastenable lines
    let fastenerOfGroup (line, group : Group) =
        let value = System.Int64.Parse group.Value
        {
            path = file;
            line = line;
            original = value;
            value = value;
        }
    let fasteners = Seq.map fastenerOfGroup groups
    if Seq.isEmpty fasteners then None
    else
        Some {
            path = file;
            lines = Array.ofSeq lines;
            fasteners = Array.ofSeq fasteners;
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
    if isPowerOfTwo value && value > 4L then
        match randomStep generator with
            | Down -> value >>> 1
            | Stay -> value
            | Up -> value <<< 1
    else
        match randomStep generator with
            | Down -> value - 1L
            | Stay -> value
            | Up -> value + 1L

let mutateFastener
    (generator : Random) (fastener : Fastener) : Fastener =
    { fastener with value = mutateValue generator fastener.value }

let mutateFile (generator : Random) (file : File) : File =
    { file with
        fasteners = mapRandom generator
            (mutateFastener generator) file.fasteners }

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
                            (text, found.value.ToString ())
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
            (fun individual fitness ->
                match fitness with
                    | Some fitness ->
                        Some { individual = individual; fitness = fitness }
                    | None -> None)
            population
        |> Array.choose id

(* Generating and running generations. *)

let generatePopulation
    (generator : Random) (size : int) (individual : Individual)
    : Population =
    Seq.init size
        (fun _ -> mapRandom generator (mutateFile generator) individual)
        |> Array.ofSeq

let rec runGeneration
    (generator : Random)
    (generations : int)
    (initial : Individual)
    (options : CommandLineOptions)
    (population : Population)
    : Population =
    printfn "%d generations remain." generations
    let results = exercisePopulation options population
    let sorted =
        Array.sortBy (fun result -> -result.fitness) results
            |> Array.map (fun result -> result.individual)
    let oneThird = sorted.Length / 3
    let fittest = Seq.take oneThird (Array.toSeq sorted) |> Seq.toArray
    let breed (generator : Random) (a : Individual) (b : Individual)
        : Individual =
        let allTraits = Array.append a b
        shuffleInPlace generator allTraits
        Array.sub allTraits 0 ((a.Length + b.Length) / 2)
    let cross (generator : Random) (group : Individual [])
        : seq<Individual> =
        Seq.init group.Length
            (fun _ ->
                let x = randomInRange generator group.Length
                let y = randomInRange generator group.Length
                breed generator group.[x] group.[y])
    let crossed = cross generator fittest
    let mutants = generatePopulation generator oneThird initial
    let newPopulation =
        Seq.append fittest crossed |> Seq.append mutants |> Seq.toArray
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
        generatePopulation generator options.populationSize initialIndividual
    let finalPopulation =
        runGeneration generator 5 initialIndividual options initialPopulation
    finalPopulation
        |> Array.map
            (Array.map
                (fun file -> Array.map stringOfFastener file.fasteners))
        |> printfn "%A"
    0
