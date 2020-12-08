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

let jumpList = testFile |> Seq.mapi splitLine

jumpList |> Seq.toList |> List.length

let generateFlipSequence length index =
    seq {
        for i in 0..length -> i = index
    }

let testLength = jumpList |> Seq.toList |> List.length

let flipSequences length =
    seq { for i in -1..length -> i}
    |> Seq.map (fun i -> generateFlipSequence testLength i)

flipSequences testLength

let flipInstruction commandLine =
    match commandLine.Instruction with
    | Nop -> { commandLine with Instruction = Jmp }
    | Jmp -> { commandLine with Instruction = Nop }
    | Acc -> commandLine

flipSequences testLength
|> Seq.map (fun s -> Seq.map2 (fun c f -> if f then flipInstruction c else c) jumpList s)
