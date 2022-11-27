open System
open System.IO


let parseInt (str : string) =
    match Int64.TryParse str with
    | true, i -> Some(i |> uint64)
    | false, _ -> None

let preProcessBuses (busStr : string) =
    busStr.Split(',')
    |> List.ofArray
    |> List.map (fun c -> (parseInt c))
    |> List.filter (Option.isSome)
    |> List.map (Option.get)


let testFile = File.ReadLines "test.txt"
let testEarliestTimeStame = testFile |> Seq.head |> uint64
let testPreprocessedBuses = preProcessBuses (testFile |> Seq.skip 1 |> Seq.head)

testPreprocessedBuses
|> List.minBy (fun busId -> (1UL + testEarliestTimeStame / busId) * busId - testEarliestTimeStame)
|> fun busId -> ((1UL + testEarliestTimeStame / busId) * busId - testEarliestTimeStame) * busId


let realFile = File.ReadLines "input.txt"
let realEarliestTimeStame = realFile |> Seq.head |> uint64
let realPreprocessedBuses = preProcessBuses (realFile |> Seq.skip 1 |> Seq.head)

realPreprocessedBuses
|> List.minBy (fun busId -> (1UL + realEarliestTimeStame / busId) * busId - realEarliestTimeStame)
|> fun busId -> ((1UL + realEarliestTimeStame / busId) * busId - realEarliestTimeStame) * busId