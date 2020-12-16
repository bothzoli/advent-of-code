open System.IO
open System.Text.RegularExpressions

let inputFile = File.ReadLines("input.txt")

type Interval = {
    Low: int
    High: int
}

type Rule = {
    Name: string
    Interval1: Interval
    Interval2: Interval
}


let parseRule input =
    let matchGroups = Regex.Match(input, "(?<name>[a-z ]+): (?<low1>\d+)-(?<high1>\d+) or (?<low2>\d+)-(?<high2>\d+)").Groups

    {
        Name = matchGroups.["name"].Value
        Interval1 = {
            Low = matchGroups.["low1"].Value |> int
            High = matchGroups.["high1"].Value |> int
        }
        Interval2 = {
            Low = matchGroups.["low2"].Value |> int
            High = matchGroups.["high2"].Value |> int
        }
    }

let reduceRules rule1 rule2 =
    {
        Name = rule1.Name + "; " + rule2.Name
        Interval1 = {
            Low = min rule1.Interval1.Low rule2.Interval1.Low
            High = max rule1.Interval1.High rule2.Interval1.High
        }
        Interval2 = {
            Low = min rule1.Interval2.Low rule2.Interval2.Low
            High = max rule1.Interval2.High rule2.Interval2.High
        }
    }

let rules =
    inputFile
    |> Seq.takeWhile ((<>) "")

let reducedRule =
    rules
    |> Seq.map parseRule
    |> Seq.reduce reduceRules


let validateRule input rule =
    (rule.Interval1.Low <= input && input <= rule.Interval1.High) || (rule.Interval2.Low <= input && input <= rule.Interval2.High)


let getTicketNumbers (input : string) =
    input.Split(',') |> Array.toList |> List.map int


let nearbyTickets =
    inputFile
    |> Seq.skip ((rules |> Seq.length) + 5)
    |> Seq.map getTicketNumbers


let getInvalidTicketNumbers ticket =
    ticket
    |> List.filter (fun number -> validateRule number reducedRule |> not)

nearbyTickets
|> Seq.map (getInvalidTicketNumbers)
|> Seq.filter (fun s -> s <> [])
|> Seq.sumBy (List.sum)
