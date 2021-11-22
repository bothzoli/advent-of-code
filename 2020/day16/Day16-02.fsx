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

let validateRule input rule =
    (rule.Interval1.Low <= input && input <= rule.Interval1.High) || (rule.Interval2.Low <= input && input <= rule.Interval2.High)

let getTicketNumbers (input : string) =
    input.Split(',') |> Array.toList |> List.map int

let rules =
    inputFile
    |> Seq.takeWhile ((<>) "")
    |> Seq.map parseRule
    |> Seq.toList

let myTicket =
    seq {
        inputFile
        |> Seq.skip ((rules |> List.length) + 2) |> Seq.head |> getTicketNumbers }

let nearbyTickets =
    inputFile
    |> Seq.skip ((rules |> List.length) + 5)
    |> Seq.map getTicketNumbers

let allTickets = nearbyTickets |> Seq.append myTicket

let validTickets =
    allTickets
    |> Seq.where (fun ticket ->
        ticket
        |> List.map (fun number ->
            rules
            |> List.fold (fun state rule ->
                state || (validateRule number rule)) false)
        |> List.reduce (&&))
    |> Seq.toList

let rec transpose tickets =
    match tickets with
    | [] -> []
    | []::_ -> []
    | xs -> List.map List.head xs :: transpose (List.map List.tail xs)

type TicketField = {
    Rule: Rule option
    Index: int
    Values: int list
}

let transposedTickets =
    transpose validTickets
    |> List.mapi (fun i ticketValues -> { Rule = None; Index = i; Values = ticketValues})

let getPossibleRulesForNumbers rules numbers =
    rules
    |> List.filter (fun rule ->
        numbers
        |> List.fold (fun state number ->
            state && validateRule number rule) true)

let rulesExcept rules ruleName =
    rules
    |> List.filter (fun { Name = name; Interval1 = _; Interval2 = _ } -> name <> ruleName)

let ticketFieldsByPossibleRule =
    transposedTickets
    |> List.sortBy (fun ticketField -> (getPossibleRulesForNumbers rules ticketField.Values) |> List.length)

let rec solve rules ticketFields =
    let ticketFieldsByPossibleRule =
        ticketFields
        |> List.sortBy (fun ticketField -> (getPossibleRulesForNumbers rules ticketField.Values) |> List.length)

    match ticketFieldsByPossibleRule with
    | [] -> []
    | ticketFields ->
        let deductedRule =
            ticketFieldsByPossibleRule
            |> List.head
            |> fun ticketField -> ticketField.Index, getPossibleRulesForNumbers rules ticketField.Values |> List.head |> fun rule -> rule.Name

        deductedRule :: solve (rulesExcept rules (snd deductedRule)) (ticketFields |> List.tail)



solve rules ticketFieldsByPossibleRule
|> List.filter (fun (_, name) -> Regex.Match(name, "departure").Success)
|> List.map (((fst) >> fun i -> myTicket |> Seq.head |> fun ticket -> ticket.[i]) >> int64)
|> List.reduce (*)
