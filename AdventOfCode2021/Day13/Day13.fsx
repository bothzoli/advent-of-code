open System.IO

let testInput = File.ReadAllLines("day13/test.txt")
let puzzleInput = File.ReadAllLines("day13/input.txt")

type Dot = {
    X: int;
    Y: int;
}

type Fold = VerticalFold of int | HorizontalFold of int

let getDotCoordinates input =
    input
    |> Seq.takeWhile (fun line -> line <> "")
    |> Seq.map (fun line ->
        match line.Split(',') with
        | [|x; y|] -> { X = int x; Y = int y}
        | _ -> failwith "Invalid input")
    |> Set.ofSeq

let getFoldLines input =
    input
    |> Seq.skipWhile (fun line -> line <> "")
    |> Seq.tail
    |> Seq.map (fun line ->
        line.Split(' ')
        |> Seq.last)
    |> Seq.map (fun (foldLine) ->
        match foldLine.Split('=') with
        | [|"x"; x|] -> VerticalFold(int x)
        | [|"y"; y|] -> HorizontalFold(int y)
        | _ -> failwith "InvalidInput")
    |> List.ofSeq

let rec folder dotSet foldList =
    let applyFold fold dotSet = 
        dotSet
        |> Set.fold (fun state current ->
            match fold with
            | HorizontalFold y ->
                if current.Y > y
                then
                    state
                    |> Set.remove current
                    |> Set.add { current with Y = (y * 2) - current.Y}
                else state
            | VerticalFold x ->
                if current.X > x
                then
                    state
                    |> Set.remove current
                    |> Set.add { current with X = (x * 2) - current.X}
                else state) dotSet

    match foldList with
    | [] -> dotSet
    | currentFold :: remainingFolds ->
        folder (applyFold currentFold dotSet) remainingFolds

let solve1 input =
    let dotSet = input |> getDotCoordinates
    let foldList = input |> getFoldLines

    folder dotSet [(foldList.Head)]
    |> Set.count

solve1 testInput
solve1 puzzleInput

let updateAt array2D x y value =
    array2D
    |> Array.updateAt x (
        array2D[x] |> Array.updateAt y value
    )

let printResult input =
    let (columnCount, rowCount) =
        input
        |> Set.fold(fun (maxColumn, maxRow) { X = currentColumn; Y = currentRow } ->
            let maxColumn = if currentColumn > maxColumn then currentColumn else maxColumn
            let maxRow = if currentRow > maxRow then currentRow else maxRow
            (maxColumn, maxRow)
        ) (0, 0)
        |> fun (maxColumn, maxRow) -> (maxColumn + 1, maxRow + 1)

    let paper = Array.create rowCount (Array.create columnCount " ")
    input
    |> Set.fold (fun state { X = column; Y = row } ->
        updateAt state row column "█"
    ) paper
    |> Array.map (Array.reduce (+))
    |> Array.map (fun line -> "\n" + line)
    |> Array.reduce (+)

let solve2 input =
    let dotSet = input |> getDotCoordinates
    let foldList = input |> getFoldLines

    folder dotSet foldList
    |> printResult

// solve2 testInput
solve2 puzzleInput
