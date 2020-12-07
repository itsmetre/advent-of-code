open System
open System.IO
open System.Text.RegularExpressions

type Validation<'a> =
    | Valid of value: 'a
    | Invalid

type Validator<'a, 'b> = 'a -> Validation<'b>

module Validate =
    let hasLength len str =
        match String.length str with
        | len' when len' = len -> Valid str
        | len' -> Invalid


    let isNumber (str: string) =
        match Int32.TryParse str with
        | (true, _) -> Valid str
        | _ -> Invalid


    let isNumberBetween n m value =
        let isBetween n m s = let x = (int s) in n <= x && x <= m

        match isNumber value with
        | Valid num when num |> isBetween n m -> Valid value
        | _ -> Invalid


    let matches' expr str =
        let regex = Regex(expr)
        let m = regex.Match(str)
        if not m.Success then
            Invalid
        else
            let captures =
                m.Groups
                |> Seq.skip 1
                |> Seq.map (fun x -> x.Value)
                |> Seq.toList

            Valid captures


    let matches expr str =
        match matches' expr str with
        | Invalid -> Invalid
        | Valid _ -> Valid str


    let andThen<'a, 'b> (f: 'a -> Validation<'b>) (va: Validation<'a>) =
        match va with
        | Invalid -> Invalid
        | Valid x -> f x


    let height str =
        matches' @"(\d+)(cm|in)" str
        |> andThen (fun [ value; unit' ] ->
            match unit' with
            | "cm" -> value |> isNumberBetween 150 193
            | "in" -> value |> isNumberBetween 59 76
            | _ -> Invalid)

module PassportProcessing =
    type Passport = Map<string, string>

    type Field =
        { Name: string
          ValidationRules: Validator<string, string> list }


    let private splitOn (delimiter: string) (str: string) =
        str.Split(delimiter, StringSplitOptions.RemoveEmptyEntries)


    let private splitOn' (delimiter: char) (str: string) =
        str.Split(delimiter, StringSplitOptions.RemoveEmptyEntries)


    let private keyValuePair (str: string) =
        match splitOn' ':' str with
        | [| lhs; rhs |] -> Some(lhs, rhs)
        | _ -> None


    let private newline = Environment.NewLine


    let private passportSeparator = sprintf "%s%s" newline newline


    let requiredFields =
        [ { Name = "byr"
            ValidationRules =
                [ Validate.hasLength 4
                  Validate.isNumberBetween 1920 2002 ] }
          { Name = "iyr"
            ValidationRules =
                [ Validate.hasLength 4
                  Validate.isNumberBetween 2010 2020 ] }
          { Name = "eyr"
            ValidationRules =
                [ Validate.hasLength 4
                  Validate.isNumberBetween 2020 2030 ] }
          { Name = "hgt"
            ValidationRules = [ Validate.height ] }
          { Name = "hcl"
            ValidationRules = [ Validate.matches @"^#[0-9a-f]{6}$" ] }
          { Name = "ecl"
            ValidationRules = [ Validate.matches @"^(amb|blu|brn|gry|grn|hzl|oth)$" ] }
          { Name = "pid"
            ValidationRules = [ Validate.matches @"^[0-9]{9}$" ] } ]



    let processPassport (text: string): Passport =
        text
        |> splitOn newline
        |> Array.collect (splitOn' ' ')
        |> Array.choose keyValuePair
        |> Array.fold (fun acc kv -> let (field, value) = kv in Map.add field value acc) Map.empty


    let hasRequiredFields passport =
        let passportHasField field passport = passport |> Map.containsKey field.Name

        let allRulesApply value rules =
            let applyRule isValid rule =
                match rule value with
                | Invalid -> false
                | Valid _ -> isValid

            List.fold applyRule true rules

        let fieldValueIsValid (field: Field) passport =
            let value = passport |> Map.find field.Name
            let rules = field.ValidationRules
            allRulesApply value rules

        requiredFields
        |> List.forall (fun field ->
            passportHasField field passport
            && fieldValueIsValid field passport)


    let runExample () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

        File.ReadAllText(path)
        |> splitOn passportSeparator
        |> Array.map processPassport
        |> Array.filter hasRequiredFields
        |> Array.length
        |> printfn "Found %d valid passports"
