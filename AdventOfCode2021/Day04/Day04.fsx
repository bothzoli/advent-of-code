open System.IO

let puzzleInput = File.ReadAllLines("input.txt")
let testInput = File.ReadAllLines("test.txt")

let bingoNumbers (input: string array) =
    input |> Seq.head |> fun s -> s.Split(',') |> Seq.map int

type BingoField = {
    IntValue: int;
    Marked: bool
}

let stringToBingoTableSequences (input: string array) =
    let lineToBingoFieldSeq line =
        line
        |> Seq.chunkBySize 3
        |> Seq.map (fun a ->
            let intValue = System.String(a).Trim() |> int
            {
                IntValue = intValue;
                Marked = false
            })
        |> Seq.toArray

    input
    |> Seq.skip 1
    |> Seq.chunkBySize 6
    |> Seq.map (Seq.skip 1)
    |> Seq.map (Seq.map lineToBingoFieldSeq)
    |> Seq.map Seq.toArray

let isWinningtable input =
    let rowsToWin bingoTable =
        bingoTable |> Array.map (Array.forall (fun b -> b.Marked))

    let columnsToWin bingoTable =
        bingoTable |> Array.transpose |> Array.map (Array.forall (fun b -> b.Marked))
    
    [| rowsToWin input; columnsToWin input |] |> Array.concat |> Array.reduce (||)

let markValue value (bingoTable: BingoField array array) =
    let getIndexOfElement (bingoTable: BingoField array array) element =
        seq {
            for i in 0 .. bingoTable.Length - 1 do
            for j in 0 .. bingoTable.Length - 1 do
            (i, j) }
        |> Seq.filter (fun (i, j) -> (bingoTable[i][j]).IntValue = element) |> Seq.tryHead

    match getIndexOfElement bingoTable value with
    | Some (i, j) -> Array.updateAt i (Array.updateAt j ({bingoTable[i][j] with Marked = true}) bingoTable[i]) bingoTable
    | None -> bingoTable

let rec playBingo numbers tables =
    let currentNumber = numbers |> Seq.head
    let newTables = tables |> Seq.map (markValue currentNumber)
    let winningTables = newTables |> Seq.filter isWinningtable
    
    if (winningTables) |> Seq.length >= 1
    then
        (winningTables |> Seq.head, currentNumber)
    else
        playBingo (numbers |> Seq.skip 1) newTables

let rec playBingoBadly numbers tables =
    let currentNumber = numbers |> Seq.head
    let newTables = tables |> Seq.map (markValue currentNumber)
    let nonWinningTables = newTables |> Seq.filter (isWinningtable >> not)

    if (nonWinningTables |> Seq.length) = 0
    then
        (newTables |> Seq.head, currentNumber)
    else
        playBingoBadly (numbers |> Seq.skip 1) nonWinningTables

let getSolution strategy input =
    let numbers = bingoNumbers input
    let tableSequences = stringToBingoTableSequences input

    let (winningTable, lastNumber) = strategy numbers tableSequences

    let calculateScore table lastNumber =
        let tableScore =
            table
            |> Array.sumBy (fun line ->
                line |> Array.sumBy (fun e ->
                    if e.Marked then 0 else e.IntValue))
        tableScore * lastNumber

    calculateScore winningTable lastNumber

getSolution playBingo testInput
getSolution playBingo puzzleInput

getSolution playBingoBadly testInput
getSolution playBingoBadly puzzleInput
