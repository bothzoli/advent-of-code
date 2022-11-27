open System.IO


let testFile = File.ReadLines "test.txt"
let inputFile = File.ReadLines "input.txt"


let neighbourCoordinates =
    seq {
        for x in [-1; 0; 1] do
        for y in [-1; 0; 1] do
        for z in [-1; 0; 1] do
        for w in [-1; 0; 1] do
            if (not <| (x = y && y = z && z = w && w = 0)) then (x, y, z, w)
    }

let getNeighbouringCoordinates (x0, y0, z0, w0) =
    neighbourCoordinates
    |> Seq.map (fun (x, y, z, w) -> (x + x0, y + y0, z + z0, w + w0))

getNeighbouringCoordinates (10, -10, 5, -5)
|> Seq.toList

let parseInputFile input =
    let inputSet =
        input
        |> Seq.mapi (fun x line ->
            line
            |> Seq.mapi (fun y chr ->
                match chr with
                | '.' -> None
                | '#' -> Some(x - 1, y - 1, 0, 0)
                | _ -> failwith "Invalid input"))
        |> Seq.concat
        |> Seq.filter (Option.isSome)
        |> Seq.map (Option.get)
        |> Set.ofSeq
    inputSet

let testParsed = parseInputFile testFile

let getWrappingCubeSizeFolder ((minX, maxX), (minY, maxY), (minZ, maxZ), (minW, maxW)) (x, y, z, w) =
    let newMinX = if x - 1 < minX then x - 1 else minX
    let newMinY = if y - 1 < minY then y - 1 else minY
    let newMinZ = if z - 1 < minZ then z - 1 else minZ
    let newMinW = if w - 1 < minW then w - 1 else minW

    let newMaxX = if x + 1 > maxX then x + 1 else maxX
    let newMaxY = if y + 1 > maxY then y + 1 else maxY
    let newMaxZ = if z + 1 > maxZ then z + 1 else maxZ
    let newMaxW = if w + 1 > maxW then w + 1 else maxW

    ((newMinX, newMaxX), (newMinY, newMaxY), (newMinZ, newMaxZ), (newMinW, newMaxW))

let getCoordinatesOfCube ((minX, maxX), (minY, maxY), (minZ, maxZ), (minW, maxW)) =
    seq {
        for x in [minX .. maxX] do
        for y in [minY .. maxY] do
        for z in [minZ .. maxZ] do
        for w in [minW .. maxW] -> (x, y, z, w)
    }

getCoordinatesOfCube ((-1, 1), (-1, 1), (0, 0), (0, 0))

let getLiveNeighbourNumber state coordinate =
    coordinate
    |> getNeighbouringCoordinates
    |> Seq.filter (fun neighbour -> state |> Set.contains neighbour)
    |> Seq.length

let initialWrappingCubeSize = ((0, 0), (0, 0), (0, 0), (0, 0))

let getNextState inputState =
    let nextState =
        inputState
        |> Set.fold getWrappingCubeSizeFolder initialWrappingCubeSize
        |> getCoordinatesOfCube
        |> Seq.map (
            (fun coordinate -> (coordinate, inputState |> Set.contains coordinate, getLiveNeighbourNumber inputState coordinate)) >>
            (fun (coordinate, isLive, liveNeighbourCount) ->
                match isLive with
                | true -> if 2 <= liveNeighbourCount && liveNeighbourCount <= 3 then Some(coordinate) else None
                | false -> if liveNeighbourCount = 3 then Some(coordinate) else None))
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Set.ofSeq
    nextState

testParsed
|> getNextState
|> getNextState
|> getNextState
|> getNextState
|> getNextState
|> getNextState
|> Set.count

parseInputFile inputFile
|> getNextState
|> getNextState
|> getNextState
|> getNextState
|> getNextState
|> getNextState
|> Set.count
