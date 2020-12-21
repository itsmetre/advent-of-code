open System
open System.Collections.Generic
open System.IO

module EncodingError =
    type Window = Dictionary<uint64, DateTime>


    type Config =
        { IsError : uint64 -> Window -> bool
          WindowSize : int }


    let add<'k,'a> (key: 'k) (value: 'a) (dict: Dictionary<'k,'a>) =
        dict.Add(key, value)
        dict


    let remove<'k,'a> (key: 'k) (dict: Dictionary<'k,'a>) =
        dict.Remove(key)


    let contains<'k,'a> (key: 'k) (dict: Dictionary<'k,'a>) =
        dict.ContainsKey(key)


    let newWindow<'k,'a when 'k : equality> () = Dictionary<'k,'a>()


    let newWindow'<'k,'a when 'k : equality> (capacity: int) = Dictionary<'k,'a>(capacity)


    let tryFind<'k,'a when 'k : equality> (predicate: 'k -> 'a -> bool) (dict: Dictionary<'k,'a>) =
        dict |> Seq.tryFind (fun x -> predicate x.Key x.Value)


    let length<'k,'a> (dict: Dictionary<'k,'a>) =
        dict.Count


    let windowIsFull cfg window =
        length window >= cfg.WindowSize


    let removeOldest<'k, 'a when 'k : equality and 'a : comparison> (window: Dictionary<'k,'a>) =
        window
        |> Seq.sortBy (fun x -> x.Value)
        |> Seq.tail
        |> (fun xs -> Dictionary<'k,'a>(xs))        


    let now () = DateTime.UtcNow


    let (|WhenNoMoreNumbers|WhenWindowNotFull|WhenWindowIsFull|) (cfg, window, numbers) =
        match numbers with
            | [] -> WhenNoMoreNumbers
            | number :: rest when windowIsFull cfg window -> WhenWindowIsFull(cfg, window, number, rest)
            | number :: rest -> WhenWindowNotFull(cfg, window, number, rest)


    let show (window: Window) =
        window
        |> Seq.sortBy (fun x -> x.Value)
        |> Seq.map (fun x -> sprintf "%O" x.Key)
        |> fun xs -> String.Join(", ", xs)


    let findEncodingErrorWith (cfg: Config) (numbers: uint64 list) =
        let addNumberToWindow cfg window number =
            if length window < cfg.WindowSize
                then add number (now()) window
                else add number (now()) (removeOldest window)
                    
        let updateErrors errors cfg window number =
            if cfg.IsError number window
                then (number :: errors)
                else errors

        let rec find cfg errors window numbers =
            match (cfg, window, numbers) with
                | WhenNoMoreNumbers ->
                    errors
                
                | WhenWindowNotFull(cfg, window, number, rest) ->
                    let window' = addNumberToWindow cfg window number
                    find cfg errors window' rest

                | WhenWindowIsFull(cfg, window, number, rest) ->
                    let errors' = updateErrors errors cfg window number
                    let window' = addNumberToWindow cfg window number
                    find cfg errors' window' rest

        find cfg [] (newWindow' cfg.WindowSize) numbers


    let windowDoesNotHavePairThatSumTo number (window: Window) =
        window |> Seq.exists (fun x -> contains (number - x.Key) window) |> not


    let findConsecutiveNumbersThatSumTo n numbers =
        let rec takeWhileLteToSum sum acc = function
            | _ when sum = n -> List.rev acc
            | [] -> List.rev acc
            | x :: xs when (sum + x) > n -> List.rev acc
            | x :: xs -> takeWhileLteToSum (sum + x) (x::acc) xs

        let rec loop = function
            | [] -> []
            | x :: xs -> match takeWhileLteToSum (uint64 0) [] (x :: xs) with
                            | ys when List.sum ys = n -> ys
                            | _ -> loop xs


        loop numbers


    let minAndMax (x, y) n = ((min x n), (max y n))


    let partOne numbers =
        let cfg = { WindowSize = 25; IsError = windowDoesNotHavePairThatSumTo }
        findEncodingErrorWith cfg numbers


    let partTwo numbers =
        let ans = partOne numbers |> List.head
        
        numbers 
        |> findConsecutiveNumbersThatSumTo ans
        |> List.fold minAndMax (UInt64.MaxValue, UInt64.MinValue)
        |> fun (x, y) -> x + y


    let run () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        
        let numbers = File.ReadAllLines(path)
                      |> Array.map uint64
                      |> Array.toList

        numbers
        |> partOne
        |> Seq.head
        |> printfn "Part One: %d"

        numbers
        |> partTwo
        |> printfn "Part Two: %d"