open System
open System.IO
open System.Text.RegularExpressions

module Parse =
    let splitOn (separator: string) (input: string) =
        input.Split(Array.singleton separator, StringSplitOptions.None)


    let matches (regex: Regex) (text: string) =
        match regex.Match(text, 0) with
            | m when m.Success -> Some m
            | _ -> None


    let countAndColor (m: Match) =
        let count = m.Groups.["count"].Value |> int
        let color = m.Groups.["color"].Value
        (count, color)


    let outerBag input =
            let regex = Regex(@"(?<color>.*) bags contain.*\.", RegexOptions.Compiled)
            input
            |> matches regex
            |> Option.map (fun m -> m.Groups.["color"].Value)


    let innerBags (input: string) =
        let pattern = @"(?<count>\d+) (?<color>\w+\b \w+) bags?"
        let regex = Regex(pattern, RegexOptions.Compiled)
        let tokens = input |> splitOn "contain "
        match Array.get tokens 1 with
            | "no other bags." -> Array.empty
            | bagList          -> bagList
                                  |> splitOn ", "
                                  |> Array.choose (matches regex)
                                  |> Array.map countAndColor


    let rule input =
        match (outerBag input, innerBags input) with
            | (Some outer, inner) -> Some (outer, inner)
            | _                   -> None


module HandyHaversacks =
    type BagQuantity = int * string


    type Rule =
        | BagContainsNoOtherBag of name:string
        | BagMustContain of name:string * otherBags:BagQuantity array


    type RuleBook = Map<string, Rule>


    let nameOf = function
        | BagContainsNoOtherBag(name) -> name
        | BagMustContain(name, _) -> name


    let toRule rule =
        let (bag, bagsInside) = rule
        match bagsInside with
            | [| |] -> BagContainsNoOtherBag bag
            | _     -> bagsInside
                       |> Array.map BagQuantity
                       |> (fun otherBags -> BagMustContain(bag, otherBags))


    let mkRuleBook lines : RuleBook =
        let toKeyValuePair rule = (nameOf rule, rule)
        lines
        |> Array.choose Parse.rule
        |> Array.map (toRule >> toKeyValuePair)
        |> Map.ofArray


    let bagContains otherBagNames = function
        | BagContainsNoOtherBag(_) -> false
        | BagMustContain(_, otherBags) ->
            let matchesBagWeAreLookingFor b = List.contains b otherBagNames 
            Array.exists (snd >> matchesBagWeAreLookingFor) otherBags


    let bagsThatCanContain bagName (rules: RuleBook) =
        let keepIfMatches bagsToFind =
            fun acc _ rule ->
                if bagContains bagsToFind rule
                    then (acc @ [rule])
                    else acc

        let rec findBags found bagsToFind (rules: RuleBook) =
            match bagsToFind with
                | [] -> List.distinctBy nameOf found

                | _  -> let newBags = Map.fold (keepIfMatches bagsToFind) [] rules
                        let newBagNames = List.map nameOf newBags
                        findBags (found @ newBags) newBagNames rules

        findBags [] [bagName] rules


    let rec getBagsInside' rulebook bag =
        let getOtherBags = function
            | BagMustContain(name, otherBags) -> otherBags
            | _ -> Array.empty
        
        seq {
            for bag in getOtherBags bag do
                let (count, name) = bag
                for i in 1 .. count do
                    yield name
                    yield! getBagsInside' rulebook (Map.find name rulebook)
        }            


    let partOne rules =
        rules
        |> bagsThatCanContain "shiny gold"
        |> List.length
        |> printfn "%d bags can contain a shiny gold bag"


    let partTwo (rules: RuleBook) =
        rules
        |> Map.find "shiny gold"
        |> getBagsInside' rules
        |> Seq.length
        |> printfn "A shiny gold bag contains %d bags"


    let run () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        let lines = File.ReadAllLines(path)
        let rules = mkRuleBook lines
        partOne rules
        partTwo rules
    