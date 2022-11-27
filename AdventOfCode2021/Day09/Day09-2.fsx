open System.IO

let puzzleInput = File.ReadAllLines("day09/input.txt")
let testInput = File.ReadAllLines("day09/test.txt")

type Position = {
    X: int;
    Y: int;
}

let inputToPositionSet (input: string array) =
    input
    |> Seq.mapi (fun i s ->
        s.ToCharArray()
        |> Seq.mapi (fun j c ->
            if c <> '9'
            then Some { X = i; Y = j }
            else None)
        |> Seq.filter Option.isSome
        |> Seq.map (fun x -> x.Value))
    |> Seq.reduce Seq.append
    |> Set.ofSeq

let getBasin positionSet start =
    let rec getNeighbours positionSet visited (toVisit: Position list) =
        if toVisit.IsEmpty
        then visited
        else
            let current = toVisit |> List.head
            let newVisited = visited |> Set.add current
            let remainingPositions = Set.difference positionSet newVisited
            let nextToVisit =
                toVisit @
                [
                    { current with X = current.X - 1};
                    { current with X = current.X + 1};
                    { current with Y = current.Y - 1};
                    { current with Y = current.Y + 1};
                ]
                |> List.filter (fun p -> Set.contains p remainingPositions)
            
            getNeighbours remainingPositions newVisited nextToVisit
    
    getNeighbours positionSet Set.empty [start]


let solve input =
    let positions =
        input
        |> inputToPositionSet
    
    let rec partitionPositions (pos: Position Set) =
        if pos.Count < 2
        then
            []
        else 
            let start = pos |> Set.toSeq |> Seq.head
            let basin = getBasin pos start

            let remaining = Set.difference pos basin

            basin.Count :: partitionPositions remaining

    partitionPositions positions
    |> List.sortDescending
    |> List.take 3
    |> List.reduce ( * )

solve testInput

solve puzzleInput
