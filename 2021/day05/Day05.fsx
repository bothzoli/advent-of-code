open System
open System.IO
open System.Text.RegularExpressions

let puzzleInput = File.ReadAllLines("input.txt")

let matcher = Regex("(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)")

type Line = {
    X1: int;
    Y1: int;
    X2: int;
    Y2: int;
}

let stringToLine input =
    let matches = (matcher.Match input).Groups
    {
        X1 = matches["x1"].Value |> int
        Y1 = matches["y1"].Value |> int
        X2 = matches["x2"].Value |> int
        Y2 = matches["y2"].Value |> int
    }

let isNotDiagonal line =
    if line.X1 = line.X2 || line.Y1 = line.Y2
    then true
    else false

type Coordinate = {
    X: int;
    Y: int;
}

let lineToCoordinates line =
    let max a b = if a > b then a else b
    let xStep = Math.Sign(line.X2 - line.X1)
    let yStep = Math.Sign(line.Y2 - line.Y1)
    let stepCount = max (abs (line.X1 - line.X2)) (abs (line.Y1 - line.Y2))

    seq {
        for i in 0 .. stepCount do
        {
            X = line.X1 + (xStep * i)
            Y = line.Y1 + (yStep * i)
        }
    }

let coordinateMap = Map.empty<Coordinate, int>

let mapFolder state current =
    let valueOption = state |> Map.tryFind current

    match valueOption with
    | Some value -> state |> Map.add current (value + 1)
    | None -> state |> Map.add current 1

puzzleInput
|> Seq.map stringToLine
|> Seq.filter isNotDiagonal
|> Seq.map lineToCoordinates
|> Seq.concat
|> Seq.fold mapFolder coordinateMap
|> Map.filter (fun c v -> v >= 2)
|> Map.count

puzzleInput
|> Seq.map stringToLine
|> Seq.map lineToCoordinates
|> Seq.concat
|> Seq.fold mapFolder coordinateMap
|> Map.filter (fun c v -> v >= 2)
|> Map.count
