open System.IO

let testFile = File.ReadLines "test.txt" |> Seq.map int

let testMyOutlet = seq { 0; (testFile |> Seq.max) + 3 }

testMyOutlet
|> Seq.append testFile
|> Seq.sort
|> Seq.pairwise
|> Seq.map (fun (x, y) -> y - x)
|> Seq.countBy ((id))
|> Seq.fold (fun acc (x, y) ->
    match x with
    | 3 | 1 -> acc * y
    | _ -> acc) 1


let realFile = File.ReadLines "input.txt" |> Seq.map int

let myOutlet = seq { 0; (realFile |> Seq.max) + 3 }

myOutlet
|> Seq.append realFile
|> Seq.sort
|> Seq.pairwise
|> Seq.map (fun (x, y) -> y - x)
|> Seq.countBy ((id))
|> Seq.fold (fun acc (x, y) ->
    match x with
    | 3 | 1 -> acc * y
    | _ -> acc) 1


let rec tribonacciSequence (n: int64) =
    seq{1L..n}
    |> Seq.fold (fun (nm1, nm2, nm3) _ -> (nm1 + nm2 + nm3, nm1, nm2)) (1L, 0L, 0L)
    |> fun (i, _, _) -> i

tribonacciSequence 0L
tribonacciSequence 1L
tribonacciSequence 2L

testMyOutlet
|> Seq.append testFile
|> Seq.sort
|> Seq.pairwise
|> Seq.map (fun (x, y) -> y - x)
|> Seq.fold (fun (lengthOfOneSeries, numberOfOnesSoFar) e ->
    if e = 1
    then lengthOfOneSeries, numberOfOnesSoFar + 1
    else (lengthOfOneSeries |> List.append [numberOfOnesSoFar]), 0) (List.empty<int>, 0)
|> fst
|> List.filter (fun x -> x <> 0)
|> List.map (int64 >> tribonacciSequence)
|> List.fold (( * )) 1L


myOutlet
|> Seq.append realFile
|> Seq.sort
|> Seq.pairwise
|> Seq.map (fun (x, y) -> y - x)
|> Seq.fold (fun (lengthOfOneSeries, numberOfOnesSoFar) e ->
    if e = 1
    then lengthOfOneSeries, numberOfOnesSoFar + 1
    else (lengthOfOneSeries |> List.append [numberOfOnesSoFar]), 0) (List.empty<int>, 0)
|> fst
|> List.filter (fun x -> x <> 0)
|> List.map (int64 >> tribonacciSequence)
|> List.fold (( * )) 1L
