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
    | 3 -> acc * y
    | 1 -> acc * y
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
    | 3 -> acc * y
    | 1 -> acc * y
    | _ -> acc) 1

