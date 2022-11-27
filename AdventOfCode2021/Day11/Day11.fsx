open System.IO

let testInput = File.ReadAllLines("day11/test.txt")
let puzzleInput = File.ReadAllLines("day11/input.txt")

let allPositions =
    seq { for i in 0 .. 99 do (i / 10, i % 10 ) }
    |> Seq.toList

let neighbourPositions (x, y) =
    [
        (x - 1, y - 1); (x - 1, y); (x - 1, y + 1);
        (x, y - 1); (x, y + 1);
        (x + 1, y - 1); (x + 1, y); (x + 1, y + 1);
    ] |> List.filter (fun (x, y) -> x >= 0 && x <= 9 && y >= 0 && y <= 9)

type Octopus = {
    Light: int;
    IsFlashing: bool;
}

let charToOctopus char =
    match char with
    | '0' -> { Light = 0; IsFlashing = true }
    | l -> { Light = l.ToString() |> int; IsFlashing = false }

let inputToOctopuses (input: string array) =
    input
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map charToOctopus)

let increaseEnergyLevel octopus =
    if octopus.IsFlashing
    then
        octopus
    else
        {
            Light = (octopus.Light + 1) % 10;
            IsFlashing = (octopus.Light + 1) = 10
        }

let rec iterateOctopuses (octopuses: Octopus array array) (toIterate: (int * int) list) =
    if toIterate.IsEmpty
    then
        octopuses
    else
        let (x, y) = toIterate.Head
        
        let currentOctopus = octopuses[x][y]

        if currentOctopus.IsFlashing
        then
            iterateOctopuses octopuses toIterate.Tail
        else
            let currentOctopus = increaseEnergyLevel currentOctopus

            let octopuses =
                octopuses |> Array.updateAt x (
                    octopuses[x] |> Array.updateAt y currentOctopus)

            let remainingPositions = 
                let remainingPositions = toIterate.Tail

                if currentOctopus.IsFlashing
                then (neighbourPositions (x, y)) @ remainingPositions
                else remainingPositions

            iterateOctopuses octopuses remainingPositions

let countFlashing octopuses =
    octopuses
    |> Array.sumBy (Array.sumBy (fun o -> if o.IsFlashing then 1 else 0))

let clearFlashing octopuses =
    octopuses |> Array.map (Array.map (fun o -> { o with IsFlashing = false }))

let folder (count, state) _ =
    (count + countFlashing state, (iterateOctopuses state allPositions |> clearFlashing))


let solution1 input =
    seq { 1 .. 100 }
    |> Seq.fold folder (0, (input |> inputToOctopuses))
    |> fst

solution1 testInput
solution1 puzzleInput

let rec syncronize octopusState roundCount =
    if (octopusState |> Array.sumBy (Array.sumBy (fun o -> o.Light))) = 0
    then
        roundCount
    else
        syncronize ((iterateOctopuses octopusState allPositions) |> clearFlashing) (roundCount + 1)


let solution2 input =
    syncronize (input |> inputToOctopuses) 0

solution2 testInput
solution2 puzzleInput
