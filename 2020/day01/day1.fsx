open System.Collections.Generic
open System.IO

module Day1 =
    let private hasKey<'k,'v> (key: 'k) (dict: IDictionary<'k,'v>) =
        dict.ContainsKey(key)

    let private valueOf<'k,'v> (key: 'k) (dict: IDictionary<'k,'v>) =
        dict.Item key

    let private add<'k,'v> (key: 'k) (value: 'v) (dict: IDictionary<'k,'v>) =
        dict.Add(key, value)
        dict

    let productOfTwoEntriesThatSumTo sum (entries: int list) =
        let rec loop sum pairs entries =
            match entries with
                | [] -> None
                
                | (entry :: _) when pairs |> hasKey entry ->
                    let otherEntry = pairs |> valueOf entry
                    Some (entry * otherEntry)
                
                | (entry :: rest) ->
                    let otherEntry = sum - entry
                    let pairs' = pairs |> add entry otherEntry |> add otherEntry entry
                    loop sum pairs' rest

        loop sum (new Dictionary<int, int>()) entries


    let productOfThreeEntriesThatSumTo sum entries =            
        let rec loop index sum entries =
            if index >= List.length entries
                then None
                else let (left, right) = entries |> List.splitAt index
                     let entry = List.head right
                     let rest = List.append left (List.tail right)
                     match productOfTwoEntriesThatSumTo (sum - entry) rest with
                        | Some product -> Some (entry * product)
                        | None -> loop (index + 1) sum entries

        loop 0 sum entries

    let run () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        
        let entries = 
            File.ReadAllLines(path) 
            |> Array.map (fun line -> line.Trim() |> int)
            |> Array.toList

        printfn "%O" (productOfThreeEntriesThatSumTo 2020 entries)
