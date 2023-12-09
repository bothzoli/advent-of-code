open System.IO

let testInput1 =
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

let testInput2 =
    "bvwbjplbgvbhsrlpgdmjqwftvncz"

let testInput3 =
    "nppdvjthqldpwncqszvftbrmjlhg"

let testInput4 =
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

let testInput5 =
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

let puzzleInput =
    File.ReadAllText("./AdventOfCode2022/Day06/input.txt")

let solve windowSize input =
    windowSize
    + (input
       |> Seq.windowed windowSize
       |> Seq.map (Set.ofArray >> Set.count)
       |> Seq.findIndex ((=) windowSize))

solve 4 testInput1
solve 4 testInput2
solve 4 testInput3
solve 4 testInput4
solve 4 testInput5

solve 4 puzzleInput

solve 14 puzzleInput
