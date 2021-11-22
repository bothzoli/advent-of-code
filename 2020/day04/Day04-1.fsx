open System.IO
open System.Text.RegularExpressions

let allRequiredFields = [Regex("byr"); Regex("iyr"); Regex("eyr"); Regex("hgt"); Regex("hcl"); Regex("ecl"); Regex("pid"); Regex("cid")]
let requiredFields = [Regex("byr:"); Regex("iyr:"); Regex("eyr:"); Regex("hgt:"); Regex("hcl:"); Regex("ecl:"); Regex("pid:")]

let regexMatch (regex: Regex) input = regex.Match(input).Success

let validatePassport (passport: string) =
    requiredFields
    |> List.map (fun regex -> regexMatch regex passport)
    |> List.reduce (( && ))
    |> (fun b -> if b then 1 else 0)

validatePassport "asdf"

let validPassport = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
let invalidPassport = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"

validatePassport validPassport
validatePassport invalidPassport

let linesFromFile filepath =
    seq { use reader = File.OpenText filepath
          while not reader.EndOfStream
             do yield reader.ReadLine () }

let testFile = File.ReadLines "test.txt"

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
            ValidCounter = ruleState.ValidCounter + validatePassport ruleState.Rule
        }
    else
        { ruleState with Rule = (ruleState.Rule.Trim() + " " + rule).Trim() }


(seq { "" })
|> Seq.append testFile
|> Seq.fold folder initialRule


let realFile = File.ReadLines "input.txt"

(seq { "" })
|> Seq.append realFile
|> Seq.fold folder initialRule
