open System.IO

type RuleState = {
    Rule: char Set
    ValidCounter: int
}

let initialRuleState = Set.ofSeq "abcdefghijklmnopqrstuvwxyz"

let initialRule = {
    Rule = initialRuleState
    ValidCounter = 0
}


let folder initialRuleState addToRule validateRule ruleState newRule =
    if newRule = ""
    then
        {
            Rule = initialRuleState
            ValidCounter = ruleState.ValidCounter + validateRule ruleState.Rule
        }
    else
        { ruleState with Rule = addToRule ruleState.Rule newRule }



let addToRule currentRule (newRule:string) =
    Set.intersect currentRule (Set.ofSeq newRule)

let validateRule (rule: char Set) =
    rule.Count

addToRule (Set.ofSeq "ab") "aaabbbc"
|> validateRule


let setFolder = folder initialRuleState addToRule validateRule



let testFile = File.ReadLines "test.txt"

(seq { "" })
|> Seq.append testFile
|> Seq.fold setFolder initialRule



let realFile = File.ReadLines "input.txt"

(seq { "" })
|> Seq.append realFile
|> Seq.fold setFolder initialRule
