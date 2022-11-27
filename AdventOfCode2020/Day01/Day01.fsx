open System.IO

let numbers = "input.txt" |> File.ReadAllLines |> Seq.ofArray |> Seq.map (int)

// Pairs that add up to 2020

(numbers, numbers)
||> Seq.allPairs
|> Seq.filter(fun (fst, snd) -> fst + snd = 2020)
|> Seq.map(fun (fst, snd) -> fst * snd)
|> Seq.head


// 3-tuples that add up to 2020

(numbers, numbers)
||> Seq.allPairs
|> Seq.allPairs numbers
|> Seq.filter(fun (fst, (snd, thd)) -> fst + snd + thd = 2020)
|> Seq.map(fun (fst, (snd, thd)) -> fst * snd * thd)
|> Seq.head
