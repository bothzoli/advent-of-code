open System.IO

let testFile = File.ReadLines "test.txt"

type Coordinates = int * int

type Place =
    | Floor
    | Seat of IsOccupied : bool * Neighbours : Coordinates list

let isOccupiedSeat (place: Place) =
    match place with
    | Seat(isOccupied, _) -> isOccupied
    | Floor -> false

let getNeighbourCoordinates (rowLength, numberOfRows) (position: Coordinates) =
    let (y, x) = position

    let leftNeighbour = Coordinates(y, x - 1)
    let rightNeighbour = Coordinates(y, x + 1)

    let upperNeighbour = Coordinates(y - 1, x)
    let lowerNeighbour = Coordinates(y + 1, x)

    let upperLeftNeighbour = Coordinates(y - 1, x - 1)
    let upperRightNeighbour = Coordinates(y - 1, x + 1)

    let lowerLeftNeighbour = Coordinates(y + 1, x - 1)
    let lowerRightNeighbour = Coordinates(y + 1, x + 1)

    [upperLeftNeighbour; upperNeighbour; upperRightNeighbour;
    leftNeighbour; rightNeighbour;
    lowerLeftNeighbour; lowerNeighbour; lowerRightNeighbour]
    |> List.filter (fun (x, y) -> (x < 0 || x >= rowLength || y < 0 || y >= numberOfRows) |> not)

getNeighbourCoordinates (3, 3) (Coordinates(1, 1))
getNeighbourCoordinates (3, 3) (Coordinates(2, 2))
getNeighbourCoordinates (3, 3) (Coordinates(0, 0))


let charToPlace places y x c =
    let numberOfRows = places |> List.length
    let rowLength = places |> List.head |> List.length
    match c with
    | '.' -> Floor
    | 'L' -> Seat(false, getNeighbourCoordinates (numberOfRows, rowLength) (Coordinates(y, x)))
    | _ -> failwith "invalid input"

let testPlaces = testFile |> Seq.map Seq.toList |> Seq.toList

let getInitialState places =
    places
    |> List.mapi (fun y l ->
        l |> List.mapi (fun x c ->
            charToPlace places y x c))

let testInitialState = getInitialState testPlaces

testInitialState.[0].[9]

let transitionSeatState (currentState: Place list list) =
    let transitionSeatState' isOccupied neighbours =
        match isOccupied with
        | true ->
            neighbours
            |> List.map (fun (y, x) -> isOccupiedSeat currentState.[y].[x])
            |> List.filter (id)
            |> List.length
            |> (fun occupiedNeighbours ->
                if occupiedNeighbours >= 4
                then Seat(false, neighbours)
                else Seat(true, neighbours))
        | false ->
            neighbours
            |> List.map (fun (y, x) -> isOccupiedSeat currentState.[y].[x])
            |> List.filter (id)
            |> List.length
            |> (fun occupiedNeighbours ->
                if occupiedNeighbours <> 0
                then Seat(false, neighbours)
                else Seat(true, neighbours))

    currentState
    |> List.mapi (fun y l ->
        l |> List.mapi (fun x _ ->
            match currentState.[y].[x] with
            | Floor -> Floor
            | Seat(isOccupied, neighbours) -> transitionSeatState' isOccupied neighbours))

let printSeatState seatState =
    seatState
    |> List.map (fun l ->
        l |> List.map (fun p ->
            match p with
            | Seat(isOccupied, _) -> if isOccupied then '#' else 'L'
            | Floor -> '.'))

printSeatState testInitialState

transitionSeatState testInitialState
|> transitionSeatState
|> transitionSeatState
|> transitionSeatState
|> transitionSeatState
|> printSeatState

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
            | Floor -> sInner
            | Seat(isOccupied, _) -> if isOccupied then sInner + 1 else sInner) 0
        rowCount + sOuter ) 0

getFinalState testInitialState
|> countOccupiedSeats

getFinalState testInitialState
|> printSeatState

let realFile = File.ReadLines "input.txt"
let realPlaces = realFile |> Seq.map Seq.toList |> Seq.toList

let realInitialState = getInitialState realPlaces

realInitialState.[96].[0]

realInitialState
|> transitionSeatState
|> printSeatState

getFinalState realInitialState
|> countOccupiedSeats
