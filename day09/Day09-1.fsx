open System.IO

let generatePairSequence length =
    seq {
        for i in 0 .. length - 1 do
            for j in i + 1 .. length - 1 do
            yield (i, j)
    }

let testFile = File.ReadLines "test.txt" |> Seq.map (int64)


let getPairsOfArray (array: int64 array) =
    let indices = generatePairSequence array.Length

    indices
    |> Seq.map (fun (i, j) -> array.[i], array.[j])


let getSumOfPairsOfArray (array: int64 array) =
    getPairsOfArray array
    |> Seq.map (fun (i, j) -> i + j)
    |> Seq.distinct


getSumOfPairsOfArray [|1L; 2L|]
getSumOfPairsOfArray [|1L; 2L; 3L|]


let checkIfPreambleGeneratesLast (array: int64 array) =
    let preamble = array |> (Array.take (array.Length - 1))
    let numberToCheck = array |> Array.last

    getSumOfPairsOfArray preamble
    |> Seq.exists (fun x -> x = numberToCheck)


let testSolution =
    testFile
    |> Seq.windowed 6
    |> Seq.filter (checkIfPreambleGeneratesLast >> not)
    |> Seq.head
    |> Array.last



let realFile = File.ReadLines "input.txt" |> Seq.map (int64)

let solution =
    realFile
    |> Seq.windowed 26
    |> Seq.filter (checkIfPreambleGeneratesLast >> not)
    |> Seq.head
    |> Array.last



let rec partOfSequenceUntilExceeds (targetValue: int64) inputSequence maxLength =
    let sumSoFar = (inputSequence |> Seq.take maxLength) |> Seq.sum
    if sumSoFar > targetValue
    then Seq.empty
    else
        if sumSoFar = targetValue then (inputSequence |> Seq.take maxLength)
        else partOfSequenceUntilExceeds targetValue inputSequence (maxLength + 1)


let getRangeThatSumsToSolution solution inputSequence =
    seq {0 .. (inputSequence |> Seq.length) - 1}
    |> Seq.map (fun i -> partOfSequenceUntilExceeds solution (inputSequence |> Seq.skip i) 1)
    |> Seq.filter (Seq.isEmpty >> not)
    |> Seq.head

let testFaultySequence = getRangeThatSumsToSolution testSolution testFile

(testFaultySequence |> Seq.min) + (testFaultySequence |> Seq.max)

let faultySequence = getRangeThatSumsToSolution solution realFile

(faultySequence |> Seq.min) + (faultySequence |> Seq.max)
