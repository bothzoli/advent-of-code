open System.IO
open System.Text.RegularExpressions

let testInput = File.ReadAllLines("2021/day12/test.txt")
let puzzleInput = File.ReadAllLines("2021/day12/input.txt")

type Cave = Start | End | Big of string | Small of string

let startsWithUpper input =
    if Regex.Match(input, "^[A-Z]").Success
    then true
    else false

let stringToCave input =
    match input with
    | "start" -> Start
    | "end" -> End
    | s -> if s |> startsWithUpper then Big(s) else Small(s)

let lineToPositionPairs (line:string) =
    let patchMatcher = Regex("(?<from>\S+)-(?<to>\S+)")
    let fromCave = patchMatcher.Match(line).Groups.["from"].Value |> stringToCave
    let toCave = patchMatcher.Match(line).Groups.["to"].Value |> stringToCave

    [(fromCave, toCave); (toCave, fromCave)]

let inputToCaveMap input =
    input
    |> Seq.map lineToPositionPairs
    |> Seq.toList
    |> List.reduce ( @ )
    |> List.groupBy fst
    |> List.map (fun (from, toList) -> (from, toList |> List.map snd))
    |> Map.ofList

let solve input =
    let caveMap = input |> inputToCaveMap
    let visitedInitial = Set.empty.Add Start

    let getNextPossibleCaves fromCave visitedSet =
        let notInVisitedSet input = not <| (visitedSet |> Set.contains input)

        caveMap
        |> Map.find fromCave
        |> List.filter notInVisitedSet

    let rec getPossibleRoutesFrom fromCave visitedSet route (routes: Cave list list) =
        getNextPossibleCaves fromCave visitedSet
        |> List.map (fun head ->
            match head with
            | Small(_) as c -> (getPossibleRoutesFrom c (visitedSet.Add c) (c :: route) routes)
            | Big(_) as c -> (getPossibleRoutesFrom c visitedSet (c :: route) routes)
            | End -> (End :: route) :: routes
            | Start -> failwith "Not possible"
        )
        |> List.concat

    getPossibleRoutesFrom Start visitedInitial [Start] List.empty
    |> List.map List.rev

solve testInput
|> List.length

solve puzzleInput
|> List.length
