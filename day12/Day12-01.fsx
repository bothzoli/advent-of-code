open System.IO


type Direction = North | East | South | West

type Instruction =
    | Move of Direction * int
    | MoveForward of int
    | TurnRight
    | TurnLeft
    | TurnAround
    | DoNothing

let turnRight direction =
    match direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft direction =
    match direction with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let flipDirection direction =
    match direction with
    | North ->South
    | East -> West
    | South -> North
    | West -> East


[ North; East; South; West]
|> List.map flipDirection

[ North; East; South; West]
|> List.map turnRight

[ North; East; South; West]
|> List.map turnLeft


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
        | 0 -> DoNothing
        | 90 -> TurnLeft
        | 180 -> TurnAround
        | 270 -> TurnRight
        | _ -> failwith "Invalid input"
    | 'R' ->
        match (distance % 360) with
        | 0 -> DoNothing
        | 90 -> TurnRight
        | 180 -> TurnAround
        | 270 -> TurnLeft
        | _ -> failwith "Invalid input"
    | _ -> failwith "Invalid input"

parseInstruction "N15"
parseInstruction "E15"
parseInstruction "S15"
parseInstruction "W15"
parseInstruction "N15"
parseInstruction "F15"

parseInstruction "L90"
parseInstruction "L180"
parseInstruction "L270"
parseInstruction "L360"
parseInstruction "L450"

parseInstruction "R90"
parseInstruction "R180"
parseInstruction "R270"
parseInstruction "R360"
parseInstruction "R450"


let moveShip direction distance (x, y) =
    match direction with
    | North -> (x + distance, y)
    | East  -> (x,            y + distance)
    | South -> (x - distance, y)
    | West  -> (x,            y - distance)

let origin = (0, 0)

moveShip North 3 origin
moveShip West 3 origin

let applyInstruction (currentPosition, currentDirection) instruction =
    match instruction with
    | Move(direction, distance) -> (moveShip direction distance currentPosition, currentDirection)
    | MoveForward(distance)     -> (moveShip currentDirection distance currentPosition, currentDirection)
    | TurnRight                 -> (currentPosition, turnRight currentDirection)
    | TurnLeft                  -> (currentPosition, turnLeft currentDirection)
    | TurnAround                -> (currentPosition, flipDirection currentDirection)
    | DoNothing                 -> (currentPosition, currentDirection)

let manhattanDistance (x, y) = abs x + abs y

let testInput = File.ReadLines "test.txt"

testInput
|> Seq.map parseInstruction
|> Seq.fold applyInstruction (origin, East)
|> fst
|> manhattanDistance

let realInput = File.ReadLines "input.txt"

realInput
|> Seq.map parseInstruction
|> Seq.fold applyInstruction (origin, East)
|> fst
|> manhattanDistance
