open System.IO
open System.Text.RegularExpressions

let testInput =
    File.ReadAllLines("./AdventOfCode2022/Day05/test.txt")

let puzzleInput =
    File.ReadAllLines("./AdventOfCode2022/Day05/input.txt")

let testInputMap =
    Map.empty<int, char list>
    |> (fun m -> m.Add(1, [ 'N'; 'Z' ]))
    |> (fun m -> m.Add(2, [ 'D'; 'C'; 'M' ]))
    |> (fun m -> m.Add(3, [ 'P' ]))

let puzzleInputMap =
    Map.empty<int, char list>
    |> (fun m -> m.Add(1, [ 'W'; 'P'; 'G'; 'Z'; 'V'; 'S'; 'B' ]))
    |> (fun m -> m.Add(2, [ 'F'; 'Z'; 'C'; 'B'; 'V'; 'J' ]))
    |> (fun m -> m.Add(3, [ 'C'; 'D'; 'Z'; 'N'; 'H'; 'M'; 'L'; 'V' ]))
    |> (fun m -> m.Add(4, [ 'B'; 'J'; 'F'; 'P'; 'Z'; 'M'; 'D'; 'L' ]))
    |> (fun m -> m.Add(5, [ 'H'; 'Q'; 'B'; 'J'; 'G'; 'C'; 'F'; 'V' ]))
    |> (fun m -> m.Add(6, [ 'B'; 'L'; 'S'; 'T'; 'Q'; 'F'; 'G' ]))
    |> (fun m -> m.Add(7, [ 'V'; 'Z'; 'C'; 'G'; 'L' ]))
    |> (fun m -> m.Add(8, [ 'G'; 'L'; 'N' ]))
    |> (fun m -> m.Add(9, [ 'C'; 'H'; 'F'; 'J' ]))

let applyStep moveItemsTogether numberOfItemsToMove fromColumn toColumn stackOfCratesMap =
    let cratesToMove =
        Map.find fromColumn stackOfCratesMap
        |> List.take numberOfItemsToMove
        |> fun l ->
            if moveItemsTogether then
                l
            else
                List.rev l

    stackOfCratesMap
    |> Map.change toColumn (fun targetColumn ->
        match targetColumn with
        | Some stack -> Some(cratesToMove @ stack)
        | None -> None)
    |> Map.change fromColumn (fun sourceColumn ->
        match sourceColumn with
        | Some stack -> Some(stack |> List.skip numberOfItemsToMove)
        | None -> None)

let crane9000 = applyStep false
let crane9001 = applyStep true

let solve craneModel input inputMap =
    let lineToInstruction line =
        let matcher =
            Regex("move (?<NumberOfItemsToMove>\d+) from (?<FromColumn>\d+) to (?<ToColumn>\d+)")

        let matches = matcher.Match line

        matches.Groups["NumberOfItemsToMove"].Value |> int,
        matches.Groups["FromColumn"].Value |> int,
        matches.Groups["ToColumn"].Value |> int

    let getInstructions input =
        input
        |> Seq.skipWhile (fun line -> line <> "")
        |> Seq.skip 1
        |> Seq.map lineToInstruction

    getInstructions input
    |> Seq.fold (fun s currentInstruction -> s |> (currentInstruction |||> craneModel)) inputMap
    |> Map.values
    |> Seq.map (List.head >> string)
    |> Seq.reduce (+)

solve crane9000 testInput testInputMap
solve crane9000 puzzleInput puzzleInputMap

solve crane9001 testInput testInputMap
solve crane9001 puzzleInput puzzleInputMap
