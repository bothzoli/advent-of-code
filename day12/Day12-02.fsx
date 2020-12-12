open System.IO
open System.Text.RegularExpressions

type WayPoint = int * int

type Direction = North | East | South | West

type Instruction =
    | MoveNorth of int
    | MoveEast of int
    | MoveSouth of int
    | MoveWest of int
    | MoveForward of int
    | TurnRight
    | TurnLeft
    | TurnAround
    | DoNothing

let turnRight (x, y) =
    WayPoint (y, -x)

let turnLeft (x, y) =
    WayPoint (-y, x)

let flipDirection (x, y) =
    WayPoint (-x, -y)

WayPoint (10, 4)
|> turnLeft
|> turnLeft
|> turnLeft
|> turnRight
|> turnRight
|> turnRight
|> flipDirection



let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (m.Groups.[1].Value, m.Groups.[2].Value |> int)
   else None


let parseInstruction input =
    match input with
    | ParseRegex "^(\w)(\d+)" (direction, distance) ->
        match direction with
        | "N" -> MoveNorth distance
        | "E" -> MoveEast distance
        | "S" -> MoveSouth distance
        | "W" -> MoveWest distance
        | "F" -> MoveForward distance
        | "L" ->
            match (distance % 360) with
            | 90 -> TurnLeft
            | 180 -> TurnAround
            | 270 -> TurnRight
            | 0 -> DoNothing
            | _ -> failwith "Invalid input"
        | "R" ->
            match (distance % 360) with
            | 90 -> TurnRight
            | 180 -> TurnAround
            | 270 -> TurnLeft
            | 0 -> DoNothing
            | _ -> failwith "Invalid input"
        | _ -> failwith "Invalid input"
    | _ -> failwith "Invalid input"


let moveShip wayPoint distance currentPosition =
    let (dx, dy) = wayPoint
    let (x, y) = currentPosition

    (x + distance * dx, y + distance * dy)

let adjustWayPoint wayPoint direction distance =
    let (dx, dy) = wayPoint
    match direction with
    | North -> (dx, dy + distance)
    | East  -> (dx + distance, dy)
    | South -> (dx, dy - distance)
    | West  -> (dx - distance, dy)

let origin = (0, 0)
let initialWayPoint = (10, 1)

moveShip initialWayPoint 10 origin

adjustWayPoint initialWayPoint North 3

let applyInstruction (currentPosition, currentWayPoint) instruction =
    match instruction with
    | MoveNorth(distance)   -> (currentPosition, adjustWayPoint currentWayPoint North distance)
    | MoveEast(distance)    -> (currentPosition, adjustWayPoint currentWayPoint East distance)
    | MoveSouth(distance)   -> (currentPosition, adjustWayPoint currentWayPoint South distance)
    | MoveWest(distance)    -> (currentPosition, adjustWayPoint currentWayPoint West distance)
    | MoveForward(distance) -> (moveShip currentWayPoint distance currentPosition, currentWayPoint)
    | TurnRight             -> (currentPosition, turnRight currentWayPoint)
    | TurnLeft              -> (currentPosition, turnLeft currentWayPoint)
    | TurnAround            -> (currentPosition, flipDirection currentWayPoint)
    | DoNothing             -> (currentPosition, currentWayPoint)

let manhattanDistance (x, y) = abs x + abs y

let testInput = File.ReadLines "test.txt"

testInput
|> Seq.map parseInstruction
|> Seq.fold applyInstruction (origin, initialWayPoint)
|> fst
|> manhattanDistance

let realInput = File.ReadLines "input.txt"

realInput
|> Seq.map parseInstruction
|> Seq.fold applyInstruction (origin, initialWayPoint)
|> fst
|> manhattanDistance
