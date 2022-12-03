open System.IO

let testInput = File.ReadAllLines("./AdventOfCode2022/Day02/test.txt")
let puzzleInput = File.ReadAllLines("./AdventOfCode2022/Day02/input.txt")

type RPS = Rock | Paper | Scissors

type Outcome = Lose | Draw | Win

let solve input =
    let lineToRPS (line:string) =
        let stringToRPS str =
            match str with
            | "A" -> Rock
            | "B" -> Paper
            | "C" -> Scissors
            | _ -> failwith "Invalid input"

        let stringToOutcome str =
            match str with
            | "X" -> Lose
            | "Y" -> Draw
            | "Z" -> Win
            | _ -> failwith "Invalid input"

        let lineSplit = line.Split(" ")
        (stringToRPS lineSplit.[0], stringToOutcome lineSplit.[1])

    let scoreRPSGame (l, r) =
        let getMyShape strategy opponent =
            let getWinningShape opponent =
                match opponent with
                | Rock -> Paper
                | Paper -> Scissors
                | Scissors -> Rock

            let getDrawingShape opponent =
                match opponent with
                | Rock -> Rock
                | Paper -> Paper
                | Scissors -> Scissors

            let getLosingShape opponent =
                match opponent with
                | Rock -> Scissors
                | Paper -> Rock
                | Scissors -> Paper

            match strategy with
            | Win -> getWinningShape opponent
            | Draw -> getDrawingShape opponent
            | Lose -> getLosingShape opponent

        let scoreOutcome outcome =
            match outcome with
            | Win -> 6
            | Draw -> 3
            | Lose -> 0

        let scoreRPS rps =
            match rps with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

        scoreOutcome r + scoreRPS (getMyShape r l)

    input
    |> Seq.map lineToRPS
    |> Seq.map scoreRPSGame
    |> Seq.sum

solve testInput
solve puzzleInput
