open System
open System.IO

let testFile = File.ReadLines "test.txt"

let boardingMapping input =
    match input with
    | 'F' -> "0"
    | 'B' -> "1"
    | 'R' -> "1"
    | 'L' -> "0"
    | _ -> failwith "invalid code"

boardingMapping 'F'
boardingMapping 'B'
boardingMapping 'R'
boardingMapping 'L'

let fromBoardingToBinary (boardingPass: string) =
    boardingPass
    |> Seq.map boardingMapping
    |> String.concat ""

fromBoardingToBinary "BFFFBBFRRR"

let binaryToDecimal input =
    Convert.ToInt64(input, 2)

fromBoardingToBinary "BFFFBBFRRR"
|> binaryToDecimal

let fromBoardingToDecimal = fromBoardingToBinary >> binaryToDecimal

testFile
|> Seq.map fromBoardingToDecimal
|> Seq.max

let realFile = File.ReadLines "input.txt"

realFile
|> Seq.map fromBoardingToDecimal
|> Seq.max


let getMissingFromThrees input =
    match input with
    | [|a; b; c |] -> not (a + 1L = b && b + 1L = c)

realFile
|> Seq.map fromBoardingToDecimal
|> Seq.sort
|> Seq.windowed 3
|> Seq.filter getMissingFromThrees
|> Seq.head
