open System.IO

let testFile = File.ReadLines "test.txt"

type Coordinates = int * int

type Place =
    | Floor
    | Seat of IsOccupied : bool

let isOccupiedSeat (place: Place) =
    match place with
    | Seat(isOccupied) -> isOccupied
    | Floor -> false

let charToPlace c =
    match c with
    | '.' -> Floor
    | 'L' -> Seat(false)
    | _ -> failwith "invalid input"

let testPlaces = testFile |> Seq.map Seq.toList |> Seq.toList

let getInitialState places =
    places
    |> List.map (fun l ->
        l |> List.map (charToPlace))

let getOccupiedNeighbourCount currentState y x =
    let rowLength = currentState |> List.length
    let numberOfRows = currentState |> List.head |> List.length

    let leftNeighbour = Coordinates(y, x - 1)
    let rightNeighbour = Coordinates(y, x + 1)

    let upperNeighbour = Coordinates(y - 1, x)
    let lowerNeighbour = Coordinates(y + 1, x)

    let upperLeftNeighbour = Coordinates(y - 1, x - 1)
    let upperRightNeighbour = Coordinates(y - 1, x + 1)

    let lowerLeftNeighbour = Coordinates(y + 1, x - 1)
    let lowerRightNeighbour = Coordinates(y + 1, x + 1)

    let neighbours =
        [upperLeftNeighbour; upperNeighbour; upperRightNeighbour;
        leftNeighbour; rightNeighbour;
        lowerLeftNeighbour; lowerNeighbour; lowerRightNeighbour]
        |> List.filter (fun (x, y) -> (x < 0 || x >= rowLength || y < 0 || y >= numberOfRows) |> not)
    
    neighbours
    |> List.map (fun (y, x) -> isOccupiedSeat currentState.[y].[x])
            |> List.filter (id)
            |> List.length

let testInitialState = getInitialState testPlaces

let transitionSeatState (currentState: Place list list) =
    let transitionSeatState' y x isOccupied =
        match isOccupied with
        | true ->
            getOccupiedNeighbourCount currentState y x
            |> (fun occupiedNeighbours ->
                if occupiedNeighbours >= 4
                then Seat(false)
                else Seat(true))
        | false ->
            getOccupiedNeighbourCount currentState y x
            |> (fun occupiedNeighbours ->
                if occupiedNeighbours <> 0
                then Seat(false)
                else Seat(true))

    currentState
    |> List.mapi (fun y l ->
        l |> List.mapi (fun x _ ->
            match currentState.[y].[x] with
            | Floor -> Floor
            | Seat(isOccupied) -> transitionSeatState' y x isOccupied))

let printSeatState seatState =
    seatState
    |> List.map (fun l ->
        l |> List.map (fun p ->
            match p with
            | Seat(isOccupied) -> if isOccupied then '#' else 'L'
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
            | Seat(isOccupied) -> if isOccupied then sInner + 1 else sInner) 0
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
