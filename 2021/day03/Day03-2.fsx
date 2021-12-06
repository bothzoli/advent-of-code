open System.IO

let input = File.ReadAllLines("input.txt")

let stringToIntArray input =
    input
    |> Seq.map (fun x -> (x |> int) - 48)
    |> Seq.toArray

let mostCommonBitAt position input =
    let cutoffPoint = (input |> Array.length |> float) / 2.0

    input
    |> Array.map (fun line ->
        line
        |> Array.skip position
        |> Array.take 1
        |> (fun x -> x[0]))
        |> Seq.sum
        |> (fun x -> if (x |> float) >= cutoffPoint then 1 else 0)

let leastCommonBitAt position input =
    if mostCommonBitAt position input = 1 then 0 else 1

let getOxigenGeneratorRating input =
    let inputAsIntArray =  input |> Array.map stringToIntArray
    let rec filterByMostCommonAt position (input: int array array) =
        if input.Length = 1
            then input
            else
                let filteredInput = input |> Array.filter (fun x -> x[position] = mostCommonBitAt position input)
                filterByMostCommonAt (position + 1) filteredInput
    
    filterByMostCommonAt 0 inputAsIntArray
    |> Array.head
    |> Array.map string
    |> Array.reduce (+)
    |> fun x -> System.Convert.ToInt32(x, 2)

let getCO2ScrubberRating input =
    let inputAsIntArray =  input |> Array.map stringToIntArray
    let rec filterByLeastCommonAt position (input: int array array) =
        if input.Length = 1
            then input
            else
                let filteredInput = input |> Array.filter (fun x -> x[position] = leastCommonBitAt position input)
                filterByLeastCommonAt (position + 1) filteredInput
    
    filterByLeastCommonAt 0 inputAsIntArray
    |> Array.head
    |> Array.map string
    |> Array.reduce (+)
    |> fun x -> System.Convert.ToInt32(x, 2)
    
let getResult input =
    getOxigenGeneratorRating input * getCO2ScrubberRating input

getResult input