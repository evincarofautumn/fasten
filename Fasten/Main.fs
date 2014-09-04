open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Text.RegularExpressions

(* Types. *)

type Column = uint32
type DirectoryPath = string
type FilePath = string
type Length = uint32
type Line = uint32
type Value = int64

[<NoComparison>]
type CommandLineOptions = {
    fastenableRegex : Regex;
    fileRegex : Regex;
    fitnessProcedure : option<unit -> string>;
    populationSize : int;
    resetProcedure : option<unit -> string>;
}

type Fastener = {
    line : Line;
    column : Column;
    length : Length;
    value : Value;
}

[<NoComparison>]
type File = {
    path : FilePath;
    lines : (int * string) [];
    fasteners : Fastener [];
}

type Population = File [] []

(* Things that should exist. *)

let cointoss (generator : Random) =
    generator.NextDouble () < 0.5

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
    randomInRange generator xs.Length |> mapIndex f xs

(* Error reporting utilities. *)

let reportUsage () : unit =
    errorfn
        "Usage: fasten [--files <regex>] --fitness <command> --reset <command> <directory>..."
    exit 1

let reportError (``exception`` : 'a) (message : string) : 'b =
    errorfn "Error: %s\n%s" (``exception``.ToString ()) message
    exit 1

let reportDirectoryNotFound (name : DirectoryPath) : 'a =
    errorfn "Directory not found: %s" name
    exit 1

let reportFailedCommand (command : string) : 'a =
    errorfn "Command failed: %s" command
    exit 1

let reportInvalidFlag (flag : string) (expected : string) : 'a =
    errorfn  "Invalid flag %s; expected %s" flag expected
    exit 1

(* Running external processes. *)

let runCommand (command : string) () =
    let mutable ``process`` = new Process ()
    (* FIXME This regex is brittle, but I didn’t want to add separate
        command-line options for process arguments. *)
    let ``match`` = Regex.Match (command, "^(\\S+)(.*)$")
    let executable = ``match``.Groups.[1].Value
    printfn "Running executable: '%s'" executable
    let arguments = ``match``.Groups.[2].Value
    ``process``.StartInfo.FileName <- executable
    ``process``.StartInfo.Arguments <- arguments
    ``process``.StartInfo.UseShellExecute <- false
    ``process``.StartInfo.RedirectStandardOutput <- true
    ``process``.StartInfo.RedirectStandardError <- true
    try
        if ``process``.Start ()
            then
                let output = ``process``.StandardOutput.ReadToEnd ()
                ``process``.WaitForExit ()
                output
            else reportFailedCommand command
    with e -> reportFailedCommand command

(* Command-line options. *)

let defaultOptions : CommandLineOptions = {
    fastenableRegex = new Regex ("(?<=FASTENABLE\\s*\\()\\d+(?=\\))");
    fileRegex = new Regex ("\\.c|\\.h");
    fitnessProcedure = None;
    populationSize = 20;
    resetProcedure = None;
}

let parseCommandLineOptions
    : string list -> CommandLineOptions * string list =
    let rec
        go = fun ((options, unused) as acc) -> function
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
        if ``match``.Success && ``match``.Groups.Count >= 1
            then Some (index, ``match``.Groups.[0])
            else None
    let groups = Seq.choose fastenable lines
    let fastenerOfGroup (line, group : Group) = {
        Fastener.line = uint32 line;
        Fastener.column = uint32 group.Index;
        Fastener.length = uint32 group.Length;
        Fastener.value = System.Int64.Parse group.Value;
    }
    let fasteners = Seq.map fastenerOfGroup groups
    if Seq.isEmpty fasteners
        then None
        else Some {
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
    if isPowerOfTwo value
        then if cointoss generator then value >>> 1 else value <<< 1
        else if cointoss generator then value - 1L else value + 1L

let mutateFastener
    (generator : Random) (fastener : Fastener) : Fastener =
    { fastener with value = mutateValue generator fastener.value }

let mutateFile (generator : Random) (file : File) : File =
    { file with
        fasteners = mapRandom generator
            (mutateFastener generator) file.fasteners }

(* Fitness testing. *)

let exercisePopulation
    (options : CommandLineOptions) (population : Population) : Population =
    options.resetProcedure.Value () |> ignore
    let writeFile file = ()  (* TODO Implement file patching. *)
    let writeIndividual = Array.iter writeFile
    let writePopulation = Array.iter writeIndividual population
    options.fitnessProcedure.Value () |> ignore
    population

(* Entry point. *)

[<EntryPoint>]
let main (argv : string []) : int =
    let options, directories = Array.toList argv |> parseCommandLineOptions
    if List.isEmpty directories
        || options.fitnessProcedure.IsNone
        || options.resetProcedure.IsNone
        then reportUsage ()
    let files =
        List.map (readDirectory options) directories
            |> Seq.concat |> Array.ofSeq
    let generator = new Random ()
    (* Since we only have one example individual—the initial source tree—we
        generate an initial population by mutating the original, which is
        assumed to be reasonably fit already. *)
    let initialPopulation =
        Seq.init options.populationSize
            (fun _ -> mapRandom generator (mutateFile generator) files)
            |> Array.ofSeq
    (*
        for each generation:
            take the fitness of each individual
            sort the individuals by fitness
            keep the top 50%
            fill the remaining 50% of the new population with:
                10% mutation
                90% 50-50 crossover
    *)
    let population = exercisePopulation options initialPopulation
    0
