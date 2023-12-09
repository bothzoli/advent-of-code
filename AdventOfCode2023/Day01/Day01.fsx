open System.IO

let testInput = File.ReadAllLines("./AdventOfCode2023/Day01/test.txt")
let test2Input = File.ReadAllLines("./AdventOfCode2023/Day01/test2.txt")
let puzzleInput = File.ReadAllLines("./AdventOfCode2023/Day01/input.txt")

let lineToNumber (line: string): int =
    let charToInt c = int c - int '0'

    line
    |> Seq.map(charToInt)
    |> Seq.filter(fun i -> i < 10 && i > -1)
    |> fun s -> $"{s |> Seq.head}{s |> Seq.last}" |> int

let solve1 (input: string array) =
    input
    |> Seq.map lineToNumber
    |> Seq.sum

solve1 testInput
solve1 puzzleInput

let stringNumberParser (input: string): string =
    let replaceString (oldValue: string) (newValue: string) (message:string): string = message.Replace(oldValue, newValue)
    
    input
    |> replaceString "zero" "z0o"
    |> replaceString "one" "o1e"
    |> replaceString "two" "t2o"
    |> replaceString "three" "t3e"
    |> replaceString "four" "f4r"
    |> replaceString "five" "f5e"
    |> replaceString "six" "s6x"
    |> replaceString "seven" "s7n"
    |> replaceString "eight" "e8t"
    |> replaceString "nine" "n9n"

let solve2 (input: string array) =
    input
    |> Seq.map stringNumberParser
    |> Seq.map lineToNumber
    |> Seq.sum

solve2 test2Input
solve2 puzzleInput