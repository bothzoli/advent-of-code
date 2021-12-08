open System.IO

let puzzleInput = File.ReadAllText("input.txt")
let testInput = "16,1,2,0,4,2,7,1,2,14"

let inputToIntList (input: string) =
    input.Split(',') |> Seq.map int |> Seq.toList

let getSumOfDistanceFromMedian input =
    let getMedian intList =
        let medianIndex = (intList |> List.length) / 2
        (intList |> List.sort)[medianIndex]

    let getDistanceFrom target value =
        abs (value - target)

    let inputAsIntList =
        input
        |> inputToIntList

    let median = inputAsIntList |> getMedian

    inputAsIntList
    |> Seq.map (getDistanceFrom median)
    |> Seq.sum

getSumOfDistanceFromMedian puzzleInput


let getMinimumWeightedDistance input =
    let getWeightedDistanceFrom target value =
        let distance = abs (value - target)
        ((distance * (distance + 1)) / 2)

    let inputAsIntList =
        input
        |> inputToIntList

    let distanceList = List.init (inputAsIntList |> List.length) id

    distanceList |>
        Seq.map (fun m ->
        inputAsIntList
        |> Seq.map (getWeightedDistanceFrom m)
        |> Seq.sumBy id
    ) |> Seq.min

getMinimumWeightedDistance testInput
getMinimumWeightedDistance puzzleInput