open System.IO

let testInput = File.ReadAllLines("./AdventOfCode2022/Day01/test.txt")
let puzzleInput = File.ReadAllLines("./AdventOfCode2022/Day01/input.txt")

let inputLineHandler line =
    if line = "" then None else Some(line |> int)

let addLineSeparatedIntInputsFolder acc curr =
    match curr with
    | None -> (snd acc |> List.rev) :: fst acc, []
    | Some value -> (fst acc, value :: (snd acc))

let solve1 input =
    (Seq.append input (Seq.singleton ""))
    |> Seq.map inputLineHandler
    |> Seq.fold addLineSeparatedIntInputsFolder (List.empty<List<int>>, [])
    |> fst
    |> List.rev
    |> List.map (List.reduce (+))
    |> List.max

solve1 testInput
solve1 puzzleInput

let solve2 input =
    (Seq.append input (Seq.singleton ""))
    |> Seq.map inputLineHandler
    |> Seq.fold addLineSeparatedIntInputsFolder (List.empty<List<int>>, [])
    |> fst
    |> List.rev
    |> List.map (List.reduce (+))
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (+)

solve2 testInput
solve2 puzzleInput
