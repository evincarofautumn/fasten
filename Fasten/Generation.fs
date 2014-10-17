namespace Fasten
module Generation =

    open System

    open Fasten.Options
    open Fasten.Types
    open Fasten.Util

    let make
        (generator : Random) (size : int) (individual : Individual)
        : Population =
        Seq.init size
            (fun _ -> Mutate.randomIndividual generator individual)
            |> Array.ofSeq

    let rec run
        (generator : Random)
        (generations : int)
        (initial : Individual)
        (options : CommandLineOptions)
        (population : Population)
        : Population =
        printfn "%d generations remain." generations
        let results =
            Exercise.population options population
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
                (fun result -> Mutate.individual generator result.individual)
                fittest
        let newPopulation = Seq.append mutants crossed |> Seq.toArray
        if generations < 1 then newPopulation
        else
            run generator (generations - 1) initial options newPopulation
