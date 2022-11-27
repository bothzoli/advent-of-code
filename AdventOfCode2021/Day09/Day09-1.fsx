open System.IO

let puzzleInput = File.ReadAllLines("day09/input.txt")
let testInput = File.ReadAllLines("day09/test.txt")

let inputToIntArrayArray (input: string array) =
    input
    |> Array.map (fun s ->
        s.ToCharArray()
        |> Array.map string
        |> Array.map int)

let get2DArrayDimensions (input: 'a array array) = (input.Length, input[0].Length)
let get2DArrayIndexSeq input =
    let (rows, cols) = get2DArrayDimensions input

    seq {
        for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
        (i, j)
    }

let getNeighbouringValues input (row, col) =
    let (rows, columns) = get2DArrayDimensions input
    let colIndices =
        if row < 1
        then [ 1 ]
        else
            if row = rows - 1
            then [ -1 ]
            else [ -1; 1]

    let rowIndices =
        if col < 1
        then [ 1 ]
        else
            if col = columns - 1
            then [ -1 ]
            else [ -1; 1]
    
    let neighbouringIndices =
        rowIndices
        |> List.map (fun c -> (row, col + c))
        |> List.append (colIndices
            |> List.map (fun r -> (row + r, col)))
            
    neighbouringIndices
    |> List.map (fun (i, j) -> input[i][j])


let getLocalMinima input =
    let intArrayArray = input |> inputToIntArrayArray

    let isLocalMinimum (value, neighbours) =
        neighbours |> Seq.forall (fun neighbour -> neighbour > value)

    let getRiskLevel i = i + 1

    get2DArrayIndexSeq intArrayArray
    |> Seq.map (fun (i, j) ->
        (intArrayArray[i][j], getNeighbouringValues intArrayArray (i, j)))
    |> Seq.filter isLocalMinimum
    |> Seq.map (fst >> getRiskLevel)
    |> Seq.sum

getLocalMinima testInput
getLocalMinima puzzleInput
