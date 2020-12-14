open System
open System.IO

module CustomCustoms =
    let private toCharArray (str: string) = str.ToCharArray()


    let private splitIntoGroups (lines: string list) = 
        let rec loop groups group lines =
            match lines with
                | []                                                -> group :: groups
                | line :: rest when String.IsNullOrWhiteSpace(line) -> loop (group :: groups) [] rest
                | line :: rest                                      -> loop groups (line :: group) rest

        loop [] [] lines


    let private questionsAnyoneAnswered (group: string list) =
        group
        |> List.collect (toCharArray >> Array.toList)
        |> List.fold (fun acc x -> Set.add x acc) (Set.empty)


    let private questionsEveryoneAnswered (group: string list) =
        group
        |> List.map (List.singleton >> questionsAnyoneAnswered)
        |> Set.intersectMany
        


    let partOne lines = 
        lines
        |> splitIntoGroups
        |> List.sumBy (questionsAnyoneAnswered >> Set.toList >> List.length)
        |> printfn "Part 1: %d"


    let partTwo lines =
        lines
        |> splitIntoGroups
        |> List.sumBy (questionsEveryoneAnswered >> Set.toList >> List.length)
        |> printfn "Part 2: %d"


    let run () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        let lines = File.ReadAllLines(path) |> Array.toList
        partOne lines
        partTwo lines