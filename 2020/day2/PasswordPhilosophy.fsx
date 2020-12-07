open System
open System.IO
open System.Text.RegularExpressions

module PasswordPhilosophy =
    type Policy =
        | RequiresBetween of n:uint * m:uint * pattern:string
        | AppearsExactlyOnceAtOneOfTwoIndices of idx1:uint * idx2:uint * pattern:string


    type Record =
        { Policy : Policy
          Password : string }


    let parseRecord entry =
        let regex = Regex(@"(?<lo>\d+)-(?<hi>\d+)\s(?<pattern>.*):\s(?<password>.*)", RegexOptions.Compiled)

        let match' = regex.Match entry

        if not match'.Success then 
            None
        else 
            let lo = match'.Groups.["lo"].Value |> uint
            let hi = match'.Groups.["hi"].Value |> uint
            let pattern = match'.Groups.["pattern"].Value
            let password = match'.Groups.["password"].Value
            Some { Policy = AppearsExactlyOnceAtOneOfTwoIndices(lo,hi,pattern); Password = password }


    let findOccurrencesOf pattern str =
        let rec find' (pattern: string) str acc (startIndex: int) =
            if String.IsNullOrEmpty(str)
                then acc
                else let index = str.IndexOf(pattern, startIndex)
                     if (index < 0)
                        then acc
                        else find' pattern str ((uint index)::acc) (index + pattern.Length)
        
        find' pattern str [] 0


    let appearsARangeOfTimesIn str lo hi pattern =
        let occurrences = str |> findOccurrencesOf pattern |> List.length |> uint
        lo <= occurrences && occurrences <= hi


    let appearsInExactlyOneOfIndices str idx1 idx2 pattern =
        let matchesOneOfIndices idx =
            let i = idx + 1u
            i = idx1 || i = idx2
        
        str 
        |> findOccurrencesOf pattern 
        |> List.filter matchesOneOfIndices
        |> List.length
        |> ((=) 1)


    let isValid record =
        let { Policy = policy; Password = password } = record
        
        match policy with
            | RequiresBetween(lo, hi, pattern) -> 
                pattern |> appearsARangeOfTimesIn password lo hi
            
            | AppearsExactlyOnceAtOneOfTwoIndices(idx1, idx2, pattern) ->
                pattern |> appearsInExactlyOneOfIndices password idx1 idx2


    let countValidRecords records =
        records |> Array.sumBy (fun record -> if isValid record then 1u else 0u)


    let runExample () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        File.ReadAllLines(path)
        |> Array.map parseRecord
        |> Array.choose id
        |> countValidRecords
        |> printfn "Found %d valid passwords"


PasswordPhilosophy.runExample();;
