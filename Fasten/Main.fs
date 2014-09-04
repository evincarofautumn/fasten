open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

(* Types. *)

type FilePath = string
type DirectoryPath = string
type Line = uint32
type Column = uint32
type Length = uint32
type Value = int64

[<NoComparison>]
type CommandLineOptions = {
    fileRegex : Regex;
    fastenableRegex : Regex;
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

(* Things that should exist. *)

let errorfn (format : Printf.TextWriterFormat<'a>) : 'a =
    fprintfn stderr format

let pair (a : 'a) (b : 'b) : 'a * 'b =
    a, b

let zipi : IEnumerable<'a> -> seq<int * 'a> =
    Seq.mapi pair

let mapRandom (generator : Random) (f : 'a -> 'a) (xs : 'a []) : 'a [] =
    let index = abs (generator.Next ()) % xs.Length
    Array.concat
        [ Array.sub xs 0 (max 0 (index - 1))
        ; [|f (xs.[index])|]
        ; Array.sub xs (index + 1) (max 0 (xs.Length - index - 1))
        ]

(* Error reporting utilities. *)

let reportUsage () : unit =
    errorfn "Usage: fasten [-f <file pattern>] <directory>..."
    exit 1

let reportError (``exception`` : 'a) (message : string) : 'b =
    errorfn "Error: %s\n%s" (``exception``.ToString ()) message
    exit 1

let reportDirectoryNotFound (name : DirectoryPath) : 'a =
    errorfn "Directory not found: %s" name
    exit 1

let reportInvalidFlag (flag : string) (expected : string) : 'a =
    errorfn  "Invalid flag %s; expected %s" flag expected
    exit 1

(* Command-line options. *)

let defaultOptions : CommandLineOptions = {
    fileRegex = new Regex ("\\.c|\\.h");
    fastenableRegex = new Regex ("FASTENABLE\\s*\\((\\d+)\\)");
}

let parseCommandLineOptions
    : string list -> CommandLineOptions * string list =
    let rec
        go = fun ((options, unused) as acc) -> function
            | "-f" :: pattern :: rest ->
                go
                    ({ options with fileRegex = new Regex (pattern) }, unused)
                    rest
            | "-f" :: [] -> reportInvalidFlag "-f" "<pattern>"
            | x :: rest ->
                go (options, x :: unused) rest
            | [] -> acc
    go (defaultOptions, [])

(* Processing functions. *)

let processFile
    (options : CommandLineOptions) (file : FilePath) : option<File> =
    (* This is eager ('ReadAllLines' rather than 'ReadLines') in order to avoid
        too many open files in large directory trees due to lazy I/O. *)
    let lines = System.IO.File.ReadAllLines file |> zipi
    let fastenable (index, text) =
        let ``match`` = options.fastenableRegex.Match text
        if ``match``.Success && ``match``.Groups.Count >= 1
            then Some (index, ``match``.Groups.[1])
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

let processDirectory
    (options : CommandLineOptions) (directory : DirectoryPath) : seq<File> =
    printfn "Processing directory: %s\n" directory;
    let files =
        try Directory.GetFiles (directory, "*", SearchOption.AllDirectories)
        with
            | :? System.IO.DirectoryNotFoundException ->
                reportDirectoryNotFound directory
            | e -> reportError e ("Directory: " + directory)
    files
        |> Seq.filter options.fileRegex.IsMatch
        |> Seq.choose (processFile options)

(* Evolution. *)

let isPowerOfTwo (x : int64) : bool =
    x <> 0L && (x &&& (x - 1L)) = 0L

let evolveValue (generator : Random) (value : Value) : Value =
    if isPowerOfTwo value
        then
            if generator.NextDouble () < 0.5
                then value >>> 1
                else value <<< 1
        else
            if generator.NextDouble () < 0.5
                then value - 1L
                else value + 1L

let evolveFastener
    (generator : Random) (fastener : Fastener) : Fastener =
    { fastener with value = evolveValue generator fastener.value }

let evolveFile (generator : Random) (file : File) : File =
    { file with
        fasteners = mapRandom generator
            (evolveFastener generator) file.fasteners }

(* Entry point. *)

[<EntryPoint>]
let main (argv : string []) : int =
    let options, directories = Array.toList argv |> parseCommandLineOptions
    if List.isEmpty directories then reportUsage ()
    let files =
        List.map (processDirectory options) directories
        |> Seq.concat |> Array.ofSeq
    let mutations = ref 0
    let generator = new Random ()
    let evolved = mapRandom generator (evolveFile generator) files
    printfn "Original:\n%A\nEvolved:\n%A" files evolved
    0
