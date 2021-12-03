open System.IO

let input = File.ReadAllLines("input.txt")

let stringToIntArray (input: string) =
    input
    |> Seq.map (fun x -> (x |> int) - 48)
    |> Seq.toArray
