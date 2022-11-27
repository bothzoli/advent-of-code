open System.IO

let testFile = File.ReadLines "test.txt"
let realFile = File.ReadLines "input.txt"

type Instruction = Nop | Acc | Jmp

type CommandLine = {
    Instruction: Instruction
    Increment: int
    CurrentIndex: int
    NextIndex: int
}


let splitLine (index: int) (line: string) =
    let [|instruction; increment|] = line.Split ' '

    let instruction =
        match instruction with
        | "nop" -> Nop
        | "acc" -> Acc
        | "jmp" -> Jmp
        | _ -> failwith "invalid instruction"

    let increment = increment |> int

    let nextIndex =
        match instruction with
        | Nop | Acc -> index + 1
        | Jmp -> index + increment

    {
        Instruction = instruction
        Increment = increment
        CurrentIndex = index
        NextIndex = nextIndex
    }

let getAccumulatorIncrement commandLine =
    match commandLine.Instruction with
    | Acc -> commandLine.Increment
    | _ -> 0

let jumpSequence = testFile |> Seq.mapi splitLine

jumpSequence |> Seq.toList |> List.length

let generateFlipSequence length index =
    seq {
        for i in 0..length -> i = index
    }

let testLength = jumpSequence |> Seq.toList |> List.length

let flipSequences length =
    seq { for i in -1..length -> i}
    |> Seq.map (fun i -> generateFlipSequence length i)

flipSequences testLength

let flipInstruction commandLine =
    match commandLine.Instruction with
    | Nop -> { commandLine with Instruction = Jmp; NextIndex = commandLine.CurrentIndex + commandLine.Increment }
    | Jmp -> { commandLine with Instruction = Nop; NextIndex = commandLine.CurrentIndex + 1 }
    | Acc -> commandLine

jumpSequence

let isCircular jumpSequence =
    let rec isCircular (jumpList: CommandLine list) visitedIndices nextIndex =
        if visitedIndices |> Set.contains nextIndex
        then true
        else
            if nextIndex >= jumpList.Length then false
            else isCircular jumpList (visitedIndices |> Set.add nextIndex) jumpList.[nextIndex].NextIndex

    isCircular (jumpSequence |> Seq.toList) Set.empty 0

isCircular jumpSequence

let fixedSequence =
    flipSequences (jumpSequence |> Seq.length)
    |> Seq.map (fun s -> Seq.map2 (fun c f -> if f then flipInstruction c else c) jumpSequence s)
    |> Seq.filter (not << isCircular)
    |> Seq.head

let realJumpSequence = (File.ReadLines "input.txt") |> Seq.mapi splitLine

let fixedRealSequence =
    flipSequences (realJumpSequence |> Seq.length)
    |> Seq.map (fun s -> Seq.map2 (fun c f -> if f then flipInstruction c else c) realJumpSequence s)
    |> Seq.filter (not << isCircular)
    |> Seq.head

let fixedRealJumpList = fixedRealSequence |> Seq.toList


let rec getAccumulatedValue (jumpList: CommandLine list) nextIndex =
    if nextIndex >= jumpList.Length then 0
    else getAccumulatorIncrement (jumpList.[nextIndex]) + (getAccumulatedValue jumpList jumpList.[nextIndex].NextIndex)

getAccumulatedValue fixedRealJumpList 0