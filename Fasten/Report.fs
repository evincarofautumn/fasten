namespace Fasten
module Report =

    open Fasten.Types
    open Fasten.Util

    let usage () : 'a =
        errorf
            "Usage:\n\
            \n\
            \tfasten [<options>] <directories>\n\
            \n\
            Required Parameters:\n\
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
            Optional Parameters:\n\
            \n\
            \t--files <regex>\n\
            \t\tRegular expression matching file names to search.\n\
            \n\
            \t--generations <count>\n\
            \t\tNumber of generations to run (default 20).\n\
            \n\
            \t--help\n\
            \t\tPrint this help message.\n\
            \n\
            \t--population <size>\n\
            \t\tSize of a population (default 20).\n\
            \n\
            \t--timeout <milliseconds>\n\
            \t\tThe amount of time to wait for external processes before killing them.\n"
        exit 1

    let error (``exception`` : 'a) (message : string) : 'b =
        errorfn "Error: %s\n%s" (``exception``.ToString ()) message
        exit 1

    let directoryNotFound (name : DirectoryPath) : 'a =
        errorfn "Directory not found: %s" name
        exit 1

    let failedCommand (command : string) (errors : option<string>) : 'a =
        errorfn "Command failed: %s" command
        match errors with
        | Some errors -> errorfn "%s" errors
        | None -> ()
        exit 1

    let invalidFitnessOutput (fitnessOutput : string) =
        errorfn "Invalid fitness function output: '%s'" fitnessOutput
        exit 1

    let invalidFlag (flag : string) (expected : string) : 'a =
        errorfn "Invalid flag %s; expected %s." flag expected
        exit 1
