open System.IO

let isIncrease (before, after) =
    if (after - before) > 0 then true
    else false

isIncrease(100, 200)
isIncrease(200, 100)
isIncrease(200, 200)

let input = File.ReadAllLines("input.txt") |> Array.map (int)

input
|> Seq.pairwise
|> Seq.map isIncrease
|> Seq.filter id
|> Seq.length


let isIncreaseThree (before, after) =
    let beforeSum = Array.sum before
    let afterSum = Array.sum after
    isIncrease(beforeSum, afterSum)

input
|> Seq.windowed 3
|> Seq.pairwise
|> Seq.map isIncreaseThree
|> Seq.filter id
|> Seq.length
