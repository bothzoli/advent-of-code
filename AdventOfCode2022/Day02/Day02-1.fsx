open System.IO

let testInput = File.ReadAllLines("./AdventOfCode2022/Day02/test.txt")
let puzzleInput = File.ReadAllLines("./AdventOfCode2022/Day02/input.txt")

type RPS = Rock | Paper | Scissors

let solve input =
    let lineToRPS (line:string) =
        let stringToRPS str =
            match str with
            | "A" | "X" -> Rock
            | "B" | "Y" -> Paper
            | "C" | "Z" -> Scissors
            | _ -> failwith "Invalid input"

        let lineSplit = line.Split(" ")
        (stringToRPS lineSplit[0], stringToRPS lineSplit[1])

    let scoreRPSGame (l, r) =
        let scoreRPS rps =
            match rps with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

        match l with
            | Rock -> (
                match r with
                | Rock -> 3 + scoreRPS r
                | Paper -> 6 + scoreRPS r
                | Scissors -> 0 + scoreRPS r)
            | Paper -> (
                match r with
                | Rock -> 0 + scoreRPS r
                | Paper -> 3 + scoreRPS r
                | Scissors -> 6 + scoreRPS r)
            | Scissors -> (
                match r with
                | Rock -> 6 + scoreRPS r
                | Paper -> 0 + scoreRPS r
                | Scissors -> 3 + scoreRPS r)

    input
    |> Seq.map lineToRPS
    |> Seq.sumBy scoreRPSGame

solve testInput
solve puzzleInput
