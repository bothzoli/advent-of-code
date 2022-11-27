open System.IO

let puzzleInput = File.ReadAllText "day06/input.txt"
let testInput = "3,4,3,1,2"

let initializeLanternFishPopulation (input: string) =
    input.Split(',')
    |> Seq.map int
    |> Seq.fold (fun s c ->
        s |> Array.updateAt c (Array.get s c + 1UL)) (Array.replicate 9 0UL)

let evolvePopulation (start: uint64 array) =
    let zeroFishCount = start[0]
    let nextPopulation = Array.append (Array.sub start 1 8) [|zeroFishCount|]
    nextPopulation |> Array.updateAt 6 (nextPopulation[6] + zeroFishCount)

let getPupulationCount afterPeriods input =
    [1 .. afterPeriods]
    |> List.fold (fun s _ -> evolvePopulation s) (initializeLanternFishPopulation input)
    |> Array.sum

getPupulationCount 80 testInput
getPupulationCount 256 testInput

getPupulationCount 80 puzzleInput
getPupulationCount 256 puzzleInput
