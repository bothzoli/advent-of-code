open System.IO
open System.Text

let testInput =
    File.ReadAllLines("./AdventOfCode2022/Day03/test.txt")

let puzzleInput =
    File.ReadAllLines("./AdventOfCode2022/Day03/input.txt")

let strToCharSet (str: string) = Set.ofArray (str.ToCharArray())

let charToPriority chr =
    let charToInt chr =
        chr
        |> string
        |> Encoding.ASCII.GetBytes
        |> Array.head
        |> int

    chr
    |> charToInt
    |> fun i -> if i < 91 then i - 38 else i - 96

let getFirstElementOfSet set = set |> Set.toSeq |> Seq.head

let solve1 input =
    let splitLineInTwo (line: string) =
        let splitPoint = line.Length / 2
        (line.Substring(0, splitPoint), line.Substring splitPoint)

    input
    |> Seq.map (
        splitLineInTwo
        >> (fun (left, right) ->
            (strToCharSet left, strToCharSet right)
            ||> Set.intersect)
        >> getFirstElementOfSet
    )
    |> Seq.sumBy charToPriority

solve1 testInput
solve1 puzzleInput

let solve2 input =
    input
    |> Seq.map strToCharSet
    |> Seq.chunkBySize 3
    |> Seq.map (
        (Array.reduce Set.intersect)
        >> getFirstElementOfSet
    )
    |> Seq.sumBy charToPriority

solve2 testInput
solve2 puzzleInput
