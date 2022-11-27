open System.IO

type Direction =
    | Up
    | Down
    | Left
    | Right

type Position = { X: int; Y: int }

let origin = { X = 0; Y = 0 }

let parseDirection direction =
    match direction with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwith "Invalid direction"

let applyDirectionToPosition direction position =
    match direction with
    | Up -> { position with Y = position.Y + 1 }
    | Down -> { position with Y = position.Y - 1 }
    | Left -> { position with X = position.X - 1 }
    | Right -> { position with X = position.X + 1 }

type PositionState = {
    CurrentPosition: Position
    VisitedPositions: Position Set
}

let input = File.ReadAllText "input.txt"

let positionFolder positionState direction =
    let newPosition = applyDirectionToPosition direction positionState.CurrentPosition
    { CurrentPosition = newPosition; VisitedPositions = (positionState.VisitedPositions.Add newPosition) }

let initialPositionState = {
    CurrentPosition = origin;
    VisitedPositions = Set.empty.Add origin
}

input
|> Seq.map parseDirection
|> Seq.fold positionFolder initialPositionState
|> fun ps -> ps.VisitedPositions.Count

type DirectionType = Santa | RobotSanta

let mapDirectionType index =
    if (index % 2 = 0) then Santa else RobotSanta

let initialSantaPositionState = initialPositionState
let initialRobotSantaPositionState = initialPositionState

let santaDirections =
    input
    |> Seq.indexed
    |> Seq.map (fun (ind, dir) -> mapDirectionType ind, parseDirection dir)
    |> Seq.filter (fun (directionType, _) ->
        match directionType with
        | Santa -> true
        | RobotSanta -> false )
    |> Seq.map (snd)

let robotSantaDirections =
    input
    |> Seq.indexed
    |> Seq.map (fun (ind, dir) -> mapDirectionType ind, parseDirection dir)
    |> Seq.filter (fun (directionType, _) ->
        match directionType with
        | RobotSanta -> true
        | _ -> false )
    |> Seq.map (snd)


let santaVisistedPositions =
    santaDirections
    |> Seq.fold positionFolder initialSantaPositionState
    |> fun ps -> ps.VisitedPositions

let robotSantaVisistedPositions =
    robotSantaDirections
    |> Seq.fold positionFolder initialRobotSantaPositionState
    |> fun ps -> ps.VisitedPositions

(Set.union santaVisistedPositions robotSantaVisistedPositions).Count