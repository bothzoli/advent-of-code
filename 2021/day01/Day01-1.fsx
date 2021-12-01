open System.IO

let input = File.ReadAllLines("input.txt") |> Array.map (int)

let isIncrease (before, after) =
    if (after - before) > 0 then true
    else false

isIncrease(100, 200)
isIncrease(200, 100)
isIncrease(200, 200)

input
|> Seq.pairwise
|> Seq.sumBy (fun e -> if isIncrease e then 1 else 0)

let isIncreaseThree (before, after) =
    let beforeSum = Array.sum before
    let afterSum = Array.sum after
    isIncrease(beforeSum, afterSum)

input
|> Seq.windowed 3
|> Seq.pairwise
|> Seq.sumBy (fun e -> if isIncreaseThree e then 1 else 0)
