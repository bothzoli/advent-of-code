open System.IO

let testFile = File.ReadLines "test.txt"

type Coordinates = int * int

type Place =
    | Floor
    | OccupiedSeat
    | UnoccupiedSeat

let isOccupiedSeat (place: Place) =
    match place with
    | OccupiedSeat -> true
    | _ -> false

let charToPlace c =
    match c with
    | '.' -> Floor
    | 'L' -> UnoccupiedSeat
    | '#' -> OccupiedSeat
    | _ -> failwith "invalid input"

let testPlaces = testFile |> Seq.map Seq.toList |> Seq.toList

let getInitialState places =
    places
    |> List.map (fun l ->
        l |> List.map (charToPlace))

type Directions = UpLeft | Up | UpRight | Left | Right | DownLeft | Down | DownRight


let getNeighbourCoordinates currentState direction y x =
    let numberOfRows = currentState |> List.length
    let rowLength = currentState |> List.head |> List.length

    let (ny, nx) = match direction with
    | UpLeft    -> Coordinates(y - 1,   x - 1)
    | Up        -> Coordinates(y - 1,   x    )
    | UpRight   -> Coordinates(y - 1,   x + 1)
    | Left      -> Coordinates(y    ,   x - 1)
    | Right     -> Coordinates(y    ,   x + 1)
    | DownLeft  -> Coordinates(y + 1,   x - 1)
    | Down      -> Coordinates(y + 1,   x    )
    | DownRight -> Coordinates(y + 1,   x + 1)

    if (nx < 0 || nx >= rowLength || ny < 0 || ny >= numberOfRows)
    then None
    else Some(Coordinates(ny, nx))

let rec getOccupiedNeighbour currentState direction y x =
    match getNeighbourCoordinates currentState direction y x with
    | Some(ny, nx) ->
        match currentState.[ny].[nx] with
        | OccupiedSeat -> true
        | UnoccupiedSeat -> false
        | Floor -> getOccupiedNeighbour currentState direction ny nx
    | None -> false

let getOccupiedNeighbourCount currentState y x =
    [ UpLeft; Up; UpRight; Left; Right; DownLeft; Down; DownRight ]
    |> List.map (fun dir ->
        getOccupiedNeighbour currentState dir y x)
    |> List.filter (id)
    |> List.length

let testInitialState = getInitialState testPlaces

let transitionSeatState (currentState: Place list list) =
    let transitionSeatState' y x seat =
        match seat with
        | OccupiedSeat ->
            getOccupiedNeighbourCount currentState y x
            |> (fun occupiedNeighbours ->
                if occupiedNeighbours >= 5
                then UnoccupiedSeat
                else OccupiedSeat)
        | UnoccupiedSeat ->
            getOccupiedNeighbourCount currentState y x
            |> (fun occupiedNeighbours ->
                if occupiedNeighbours <> 0
                then UnoccupiedSeat
                else OccupiedSeat)
        | Floor -> Floor

    currentState
    |> List.mapi (fun y l ->
        l |> List.mapi (fun x _ ->
            match currentState.[y].[x] with
            | Floor -> Floor
            | seat -> transitionSeatState' y x seat))

let printSeatState seatState =
    seatState
    |> List.map (fun l ->
        l |> List.map (fun p ->
            match p with
            | OccupiedSeat -> '#'
            | UnoccupiedSeat -> 'L'
            | Floor -> '.'))

printSeatState testInitialState


let rec getFinalState initialState =
    let newState = transitionSeatState initialState
    if initialState = newState
    then newState
    else getFinalState newState

let countOccupiedSeats seatState =
    seatState
    |> List.fold (fun sOuter l ->
        let rowCount = l |> List.fold (fun sInner p ->
            match p with
            | OccupiedSeat -> sInner + 1
            | _ -> sInner) 0
        rowCount + sOuter ) 0

getFinalState testInitialState
|> countOccupiedSeats

getFinalState testInitialState
|> printSeatState

let realFile = File.ReadLines "input.txt"
let realPlaces = realFile |> Seq.map Seq.toList |> Seq.toList

let realInitialState = getInitialState realPlaces

getFinalState realInitialState
|> countOccupiedSeats
