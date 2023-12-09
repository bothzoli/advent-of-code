open System.IO

let testInput =
    File.ReadAllLines("./AdventOfCode2022/Day04/test.txt")

let puzzleInput =
    File.ReadAllLines("./AdventOfCode2022/Day04/input.txt")

let lineToSectionPairs (line: string) =
    let stringToSectionBorders (str: string) =
        let borders = str.Split('-') |> Array.map int
        (borders[0], borders[1])

    line.Split(',')
    |> Seq.map stringToSectionBorders
    |> fun s -> (s |> Seq.head, s |> Seq.last)

let whereSectionsFullyOverlap (section1, section2) =
    (fst section1 <= fst section2 && snd section1 >= snd section2)
     || (fst section2 <= fst section1 && snd section2 >= snd section1)

let whereSectionsOverlap (section1, section2) =
    (snd section1 >= fst section2 && snd section1 <= snd section2)
     || (snd section2 >= fst section1 && snd section2 <= snd section1)

let solve1 input =
    input
    |> Seq.map lineToSectionPairs
    |> Seq.filter whereSectionsFullyOverlap
    |> Seq.length

let solve2 input =
    input
    |> Seq.map lineToSectionPairs
    |> Seq.filter whereSectionsOverlap
    |> Seq.length

solve1 testInput
solve1 puzzleInput

solve2 testInput
solve2 puzzleInput
