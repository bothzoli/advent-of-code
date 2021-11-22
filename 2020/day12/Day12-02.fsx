open System.IO


type WayPoint = int * int

type Direction = North | East | South | West

type Instruction =
    | Move of Direction * int
    | MoveForward of int
    | TurnRight
    | TurnLeft
    | TurnAround
    | DoNothing

let turnRight (x, y) = WayPoint (y, -x)

let turnLeft (x, y) = WayPoint (-y, x)

let flipDirection (x, y) = WayPoint (-x, -y)

WayPoint (10, 4)
|> turnLeft
|> turnLeft
|> turnLeft
|> turnRight
|> turnRight
|> turnRight
|> flipDirection


let parseInstruction (input : string) =
    let direction = input |> Seq.head
    let distance = input.Substring(1) |> int

    match direction with
    | 'N' -> Move (North, distance)
    | 'E' -> Move (East, distance)
    | 'S' -> Move (South, distance)
    | 'W' -> Move (West, distance)
    | 'F' -> MoveForward distance
    | 'L' ->
        match (distance % 360) with
        | 0     -> DoNothing
        | 90    -> TurnLeft
        | 180   -> TurnAround
        | 270   -> TurnRight
        | _     -> failwith "Invalid input"
    | 'R' ->
        match (distance % 360) with
        | 0     -> DoNothing
        | 90    -> TurnRight
        | 180   -> TurnAround
        | 270   -> TurnLeft
        | _     -> failwith "Invalid input"
    | _ -> failwith "Invalid input"


let moveShip (dx, dy) distance (x, y) = (x + distance * dx, y + distance * dy)

let adjustWayPoint (dx, dy) direction distance =
    match direction with
    | North -> (dx,            dy + distance)
    | East  -> (dx + distance, dy)
    | South -> (dx,            dy - distance)
    | West  -> (dx - distance, dy)

let origin = (0, 0)
let initialWayPoint = (10, 1)

moveShip initialWayPoint 10 origin

adjustWayPoint initialWayPoint North 3

let applyInstruction (currentPosition, currentWayPoint) instruction =
    match instruction with
    | Move(direction, distance) -> (currentPosition, adjustWayPoint currentWayPoint direction distance)
    | MoveForward(distance)     -> (moveShip currentWayPoint distance currentPosition, currentWayPoint)
    | TurnRight                 -> (currentPosition, turnRight currentWayPoint)
    | TurnLeft                  -> (currentPosition, turnLeft currentWayPoint)
    | TurnAround                -> (currentPosition, flipDirection currentWayPoint)
    | DoNothing                 -> (currentPosition, currentWayPoint)

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
