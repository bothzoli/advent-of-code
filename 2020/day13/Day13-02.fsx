open System
open System.IO


let parseInt (str : string) =
    match Int64.TryParse str with
    | true, i -> Some(i |> uint64)
    | false, _ -> None

let preProcessBuses (busStr : string) =
    busStr.Split(',')
    |> List.ofArray
    |> List.mapi (fun i c -> (i, parseInt c))
    |> List.filter (fun (_, num) -> Option.isSome num)
    |> List.map ((fun (i, num) -> i |> uint64, Option.get num) >> (fun (i, num) -> (num - (i % num)) % num, num))
    |> List.sortByDescending snd


let testFile = File.ReadLines "test.txt"
let testBuses = testFile |> Seq.skip 1 |> Seq.head
let testPreprocessedBuses = preProcessBuses testBuses


let realFile = File.ReadLines "input.txt"
let realBuses = realFile |> Seq.skip 1 |> Seq.head
let realPreprocessedBuses = preProcessBuses realBuses


let solveCongruencies (remainder2 : uint64, modulo2 : uint64) (remainder1 : uint64, modulo1 : uint64) =
    let rec solveCongruencies' (rem1 : uint64, mod1 : uint64) (rem2 : uint64, mod2 : uint64) =
        if rem1 % mod2 = rem2
        then (rem1, mod1 * mod2)
        else solveCongruencies' (rem1 + mod1, mod1) (rem2, mod2)

    solveCongruencies' (remainder1, modulo1) (remainder2, modulo2)


testPreprocessedBuses
|> List.reduceBack solveCongruencies

realPreprocessedBuses
|> List.reduceBack solveCongruencies
