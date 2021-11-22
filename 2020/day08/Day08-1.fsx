open System.IO

let testFile = File.ReadLines "test.txt"

type Instruction = Nop | Acc | Jmp

type CommandLine = {
    Instruction: Instruction
    Increment: int
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
        NextIndex = nextIndex
    }

splitLine 0 "nop +0"

let getAccumulator commandLine =
    match commandLine.Instruction with
    | Acc -> commandLine.Increment
    | _ -> 0

let jumpList = testFile |> Seq.mapi splitLine |> Seq.toList

let getNextIndex index = jumpList.[index].NextIndex

type ProgramState = {
    VisitedIndices: int Set
    NextIndex: int
    Accumulator: int
    ChangedSincePrevious: bool
}

let initialProgramState = {
    VisitedIndices = Set.empty
    NextIndex = 0
    Accumulator = 0
    ChangedSincePrevious = true
}

let rec generateNextState (jumpList: CommandLine list) (currentState: ProgramState) =
    if currentState.ChangedSincePrevious
    then generateNextState jumpList {
            VisitedIndices = Set.add currentState.NextIndex currentState.VisitedIndices
            NextIndex = jumpList.[currentState.NextIndex].NextIndex
            Accumulator = currentState.Accumulator + getAccumulator jumpList.[currentState.NextIndex]
            ChangedSincePrevious = not <| Set.contains currentState.NextIndex currentState.VisitedIndices
        }
    else { currentState with ChangedSincePrevious = false }

let generateNextStateJL = generateNextState jumpList

generateNextStateJL initialProgramState


let realJumpList = File.ReadLines "input.txt" |> Seq.mapi splitLine |> Seq.toList

let generateNextStateRJL = generateNextState realJumpList

generateNextStateRJL initialProgramState

