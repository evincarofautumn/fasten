namespace Fasten
module Read =

    open System
    open System.IO

    open Fasten.Options
    open Fasten.Report
    open Fasten.Types
    open Fasten.Util

    let file
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

    let directory
        (options : CommandLineOptions) (directory : DirectoryPath) : seq<File> =
        let files =
            try Directory.GetFiles (directory, "*", SearchOption.AllDirectories)
            with
            | :? System.IO.DirectoryNotFoundException ->
                Report.directoryNotFound directory
            | e -> Report.error e ("Directory: " + directory)
        files
            |> Seq.filter options.fileRegex.IsMatch
            |> Seq.choose (file options)
