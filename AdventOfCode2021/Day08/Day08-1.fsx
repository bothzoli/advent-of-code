open System.IO

let puzzleInput = File.ReadAllLines("2021/day08/input.txt")

puzzleInput
|> Seq.map
    ((fun l -> l.Split('|'))
    >> Seq.last
    >> fun s -> s.Trim().Split(' ')
    >> Seq.filter (fun s -> s.Length = 2 || s.Length = 3 || s.Length = 4 || s.Length = 7)
    >> Seq.length)
|> Seq.sum
