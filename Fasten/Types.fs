namespace Fasten
module Types =

    type Command = unit -> option<string>
    type DirectoryPath = string
    type FilePath = string
    type Fitness = float
    type Line = int

    type Value =
        | Regular of int64
        | PowerOfTwo of int64
        | Boolean of bool

    let stringOfValue (value : Value) : string =
        match value with
        | Regular i -> sprintf "%d" i
        | PowerOfTwo i -> sprintf "%d" i
        | Boolean b -> if b then "1" else "0"

    type Fastener = {
        path : FilePath
        line : Line
        original : Value
        value : Value
    }

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
