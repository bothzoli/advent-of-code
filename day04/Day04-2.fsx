open System.IO
open System.Text.RegularExpressions


let validPassport = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
let invalidPassport = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"

let validateYear lowerBound higherBound (year:string) =
    let intYear = int year
    year.Length = 4 && intYear >= lowerBound && intYear <= higherBound

let validateByr = validateYear 1920 2020
let validateIyr = validateYear 2010 2020
let validateEyr = validateYear 2020 2030

validateByr "123"
validateByr "1234"
validateByr "1920"
validateByr "2000"
validateByr "2020"
validateByr "2021"
validateByr "20202"

validateIyr "123"
validateIyr "1234"
validateIyr "1920"
validateIyr "2010"
validateIyr "2020"
validateIyr "2021"
validateIyr "20202"

validateEyr "123"
validateEyr "1234"
validateEyr "1920"
validateEyr "2020"
validateEyr "2030"
validateEyr "2031"
validateEyr "20202"

let validateInRange lowerBound higherBound (input: string) =
    let intInput = int input
    intInput >= lowerBound && intInput <= higherBound

let hgtRegex = Regex "(\\d+)(cm|in)"

let matches = hgtRegex.Match "123in"
let success = matches.Success
let height = matches.Groups.[1].Value |> int
let uom = matches.Groups.[2].Value

let validateCm = validateInRange 150 193
let validateIn = validateInRange 59 76

validateCm "149"
validateCm "150"
validateCm "193"
validateCm "194"

validateIn "58"
validateIn "59"
validateIn "76"
validateIn "77"

let validateHgt input =
    let matches = input |> hgtRegex.Match

    if not matches.Success
        then false
        else
            let (height, uom) = matches.Groups.[1].Value, matches.Groups.[2].Value
            match uom with
            | "cm" -> validateCm height
            | "in" -> validateIn height
            | _ -> false

validateHgt "149"
validateHgt "cm"

validateHgt "149cm"
validateHgt "150cm"
validateHgt "193cm"
validateHgt "194cm"
validateHgt "58in"
validateHgt "59in"
validateHgt "76in"
validateHgt "77in"


let hclRegex = Regex "^#([0-9]|[a-z]){6}$"

let validateHcl input =
    hclRegex.Match(input).Success

validateHcl "#123abc"
validateHcl "#123abcb"
validateHcl "#0abcb"

let validateEcl input =
    input = "amb" || input = "blu" || input = "brn" || input = "gry" || input = "grn" || input = "hzl" || input = "oth"

validateEcl "asd"
validateEcl "amb"

let pidRegex = Regex "^[0-9]{9}$"

let validatePid input = (pidRegex.Match input).Success

validatePid "123456789"
validatePid "12345678"
validatePid "1234567890"

let requiredFields = [Regex "byr:"; Regex "iyr:"; Regex "eyr:"; Regex "hgt:"; Regex "hcl:"; Regex "ecl:"; Regex "pid:"]

let validateRequiredFields (passport: string) =
    requiredFields
    |> List.map (fun regex -> regex.Match(passport).Success)
    |> List.reduce (( && ))

validateRequiredFields validPassport
validateRequiredFields invalidPassport

let validateField (field:string) =
    let key, value =
        match field.Split(':') with
        | [| key; value |] -> key, value
        | _ -> failwith "invalid field"

    match key with
    | "byr" -> validateByr value
    | "iyr" -> validateIyr value
    | "eyr" -> validateEyr value
    | "hgt" -> validateHgt value
    | "hcl" -> validateHcl value
    | "ecl" -> validateEcl value
    | "pid" -> validatePid value
    | "cid" -> true
    | _ -> failwith "invalid key"


validateField "ecl:gry"
validateField "pid:860033327"
validateField "eyr:2020"
validateField "hcl:#fffffd"
validateField "byr:1937"
validateField "iyr:2017"
validateField "cid:147"
validateField "hgt:183cm"

let validateFields (passport:string) =
    passport.Split ' '
    |> Array.map validateField
    |> Array.reduce ((&&))

validateFields validPassport

let validate input =
    if validateRequiredFields input && validateFields input then 1 else 0

validate validPassport
validate invalidPassport



type RuleState = {
    Rule: string
    ValidCounter: int
}

let initialRule = {
    Rule = ""
    ValidCounter = 0
}


let folder ruleState rule =
    if rule = ""
    then
        {
            Rule = ""
            ValidCounter = ruleState.ValidCounter + validate ruleState.Rule
        }
    else
        { ruleState with Rule = (ruleState.Rule.Trim() + " " + rule).Trim() }


let realFile = File.ReadLines "input.txt"

(seq { "" })
|> Seq.append realFile
|> Seq.fold folder initialRule
