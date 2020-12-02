open System.IO

let numbers = "input.txt" |> File.ReadAllLines |> List.ofArray |> List.map (int)

// Pairs that add up to 2020

(numbers, numbers)
||> List.allPairs
|> List.filter(fun (fst, snd) -> fst + snd = 2020)
|> List.map(fun (fst, snd) -> fst * snd)
|> List.head


// 3-tuples that add up to 2020

(numbers, numbers)
||> List.allPairs
|> List.allPairs numbers
|> List.filter(fun (fst, (snd, thd)) -> fst + snd + thd = 2020)
|> List.map(fun (fst, (snd, thd)) -> fst * snd * thd)
|> List.head
