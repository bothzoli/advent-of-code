open System.IO

let testInput = File.ReadAllLines("2021/day15/test.txt")
let test2Input = File.ReadAllLines("2021/day15/test2.txt")
let puzzleInput = File.ReadAllLines("2021/day15/input.txt")

type Position = int * int

type Vertex = {
    Distance: int option
    Predecessor: Position option
}

type Edge = {
    From: Position
    To: Position
    Weight: int
}


let solve input =
    let rows = input |> Seq.length
    let columns = input |> Seq.head |> Seq.length

    let neighbourPositions (position: Position) =
        let (x, y) = position
        [
            Position(x - 1, y);
            Position(x, y - 1);
            Position(x, y + 1);
            Position(x + 1, y)
        ] |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < columns)
    
    let inputToVertexMap rows columns = 
        seq {
            for i in 0 .. rows - 1 do
                for j in 0 .. columns - 1 ->
                    (Position(i, j), { Distance = None; Predecessor = None })
            }
        |> Map.ofSeq

    let vertexMap = inputToVertexMap rows columns

    let inputToVerticesAndEdges (input: string seq) =

        let inputToRiskLevels (input: string seq) =
            input
            |> Seq.mapi (fun i line ->
                line
                |> Seq.mapi (fun j r ->
                    (Position(i, j), r |> string |> int)
                ))
            |> Seq.concat
            |> Map.ofSeq

        let riskLevelMap = inputToRiskLevels input

        let riskLevelsToEdges (input: Map<Position,int>) =
            input
            |> Map.map (fun source _ ->
                neighbourPositions source
                |> List.map (fun destination ->
                    {
                        From = source
                        To = destination
                        Weight = (input |> Map.find destination)
                    }
                )
            )
            |> Map.values
            |> List.concat

        let edges = riskLevelsToEdges riskLevelMap

        (vertexMap, edges)

    let (vertexMap, edgeList) = inputToVerticesAndEdges input

    let bellmanFord (vertexMap: Map<Position, Vertex>) (edgeList: Edge list) (source: Position) = 
        // vertexMap = vertexMap |> Map.add (source) ({ Distance = Some(0); Predecessor = None })
        let vertexMap = vertexMap |> Map.add (source) ({ Distance = Some(0); Predecessor = None })

        let rec iterate vertexMap edgeList iteration =
            printfn "%i" iteration
            let newVertexMap =
                edgeList
                |> List.fold (fun s c ->
                    let fromV = s |> Map.find c.From

                    match fromV.Distance with
                    | None -> s
                    | Some(fromDistance) ->
                        let toV = s |> Map.find c.To
                        match toV.Distance with
                        | Some(currentDistance) ->
                            if currentDistance < fromDistance + c.Weight
                            then s
                            else s |> Map.add c.To { Distance = Some(fromDistance + c.Weight); Predecessor = Some(c.From) }
                        | None -> s |> Map.add c.To { Distance = Some(fromDistance + c.Weight); Predecessor = Some(c.From) }
                ) vertexMap

            if vertexMap = newVertexMap
            then vertexMap
            else iterate newVertexMap edgeList (iteration + 1)

        iterate vertexMap edgeList 0

    let testMap = bellmanFord vertexMap edgeList (Position(0, 0))
    testMap |> Map.find (Position(rows - 1, columns - 1))

solve testInput

solve puzzleInput

let expanderSequence =
    seq {
        for i in 0..4 do
            yield seq { for j in 0..4 -> i + j}
    }

let increaseRiskBy increase i = ((i + increase - 1) % 9) + 1 
let charToInt c = (c |> string) |> int

let expandInput input = 
    expanderSequence
    |> Seq.map (fun increase ->
        increase
        |> Seq.map (fun i ->
            input
            |> Seq.map (fun line ->
                line
                |> Seq.map charToInt
                |> Seq.map (increaseRiskBy i)
                |> Seq.map string
                |> Seq.reduce (+)
            )
        )
    )
    |> Seq.map Seq.transpose
    |> Seq.map (Seq.map (Seq.reduce (+)))
    |> Seq.concat

solve (expandInput testInput)
solve (expandInput puzzleInput)