open System.IO
open System.Text.RegularExpressions

let pair a b = a, b
let zipi = Seq.mapi pair

let reportUsage () =
    fprintfn stderr "Usage: fasten [-f <file pattern>] <directory>..."
    exit 1

let reportError exception_ message =
    fprintfn stderr "Error: %s\n%s" (exception_.ToString ()) message
    exit 1

let reportDirectoryNotFound name =
    fprintfn stderr "Directory not found: %s" name
    exit 1

let reportInvalidFlag flag expected =
    fprintfn stderr "Invalid flag %s; expected %s" flag expected
    exit 1

[<NoComparison>]
type CommandLineOptions = {
    fileRegex : Regex;
    fastenableRegex : Regex;
}

let defaultOptions = {
    CommandLineOptions.fileRegex = new Regex ("\\.c|\\.h");
    CommandLineOptions.fastenableRegex = new Regex ("FASTENABLE");
}

let parseCommandLineOptions =
    let rec
        go = fun ((options, unused) as acc) -> function
            | "-f" :: pattern :: rest ->
                go
                    ({ options with fileRegex = new Regex (pattern) }, unused)
                    rest
            | "-f" :: [] -> reportInvalidFlag "-f" "pattern"
            | x :: rest ->
                go (options, x :: unused) rest
            | [] -> acc
    go (defaultOptions, [])

let processFile (options : CommandLineOptions) file =
    let lines = System.IO.File.ReadLines file |> zipi
    let filteredLines =
        Seq.filter (fun (index, text) -> options.fastenableRegex.IsMatch text) lines
    for index, text in filteredLines do
        printfn "%s:%d:%s" file index text

let processDirectory options directory =
    printfn "Processing directory: %s\n" directory;
    let files =
        try Directory.GetFiles (directory, "*", SearchOption.AllDirectories)
        with
            | :? System.IO.DirectoryNotFoundException -> reportDirectoryNotFound directory
            | e -> reportError e ("Directory: " + directory)
    files |> Seq.filter options.fileRegex.IsMatch |> Seq.iter (processFile options)

[<EntryPoint>]
let main argv =
    let options, directories = Array.toList argv |> parseCommandLineOptions
    if List.isEmpty directories then reportUsage ()
    List.iter (processDirectory options) directories
    0
