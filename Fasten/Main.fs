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
    value : Value;
}

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
                if ``process``.ExitCode = 0 then
                    Some (output.ToString ())
                else None
            else
                ``process``.Kill ()
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
    populationSize = 10;
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
    let fastenerOfGroup (line, group : Group) = {
        Fastener.path = file;
        Fastener.line = line;
        Fastener.value = System.Int64.Parse group.Value;
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
            for fastener in file.fasteners do
                let text =
                    if fastener.line = line then
                        options.fastenableRegex.Replace
                            (text, fastener.value.ToString ())
                    else text
                writer.WriteLine text
        writer.Flush ()
        writer.Close ()
    let writeIndividual individual =
        Array.iter writeFile individual
    let exercise individual =
        printfn "Resetting tree."
        options.resetProcedure.Value () |> ignore
        printfn "Writing individual."
        writeIndividual individual
        printfn "Building."
        options.buildProcedure.Value () |> ignore
        printfn "Testing fitness."
        let fitnessOutput = options.fitnessProcedure.Value ()
        match fitnessOutput with
            | Some output ->
                try
                    let fitness = 1.0 / System.Double.Parse output
                    printfn "Calculated fitness: %f." fitness
                    Some fitness
                with e -> None
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
    let files =
        List.map (readDirectory options) directories
            |> Seq.concat |> Array.ofSeq
    let generator = new Random ()
    (* Since we only have one example individual—the initial source tree—we
        generate an initial population by mutating the original, which is
        assumed to be reasonably fit already. *)
    printfn "Computing initial population."
    let initialPopulation =
        Seq.init options.populationSize
            (fun _ -> mapRandom generator (mutateFile generator) files)
            |> Array.ofSeq
    let results = exercisePopulation options initialPopulation
    let sorted = Array.sortBy (fun result -> result.fitness) results
    let fittest = sorted.[0]
    printfn "Fittest configuration: %A"
        (Array.collect (fun file -> file.fasteners) fittest.individual)
    (*
        for each generation:
            take the fitness of each individual
            sort the individuals by fitness
            keep the top 50%
            fill the remaining 50% of the new population with:
                10% mutation
                90% 50-50 crossover
    *)
    0
