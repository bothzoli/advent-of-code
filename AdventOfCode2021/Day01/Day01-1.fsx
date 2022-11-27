open System.IO

let input = File.ReadAllLines("input.txt") |> Array.map int

let isIncrease (before, after) = if after > before then 1 else 0

input
|> Seq.pairwise
|> Seq.sumBy isIncrease

input
|> Seq.windowed 3
|> Seq.map Array.sum
|> Seq.pairwise
|> Seq.sumBy isIncrease
