open System.IO

let puzzleInput = File.ReadAllLines("2021/day10/input.txt")
let testInput = File.ReadAllLines("2021/day10/test.txt")

let isClosing c =
    match c with
    | '(' | '[' | '{' | '<' -> false
    | _ -> true

let getClosingOf o =
    match o with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwith "Invalid character"

let t = testInput |> Seq.head

let rec solver (input: char seq) parsedLength (expectedRemaining: char list) =
    if input |> Seq.length = 0
    then None, expectedRemaining
    else
        let inputHead = input |> Seq.head
        let inputTail = input |> Seq.tail
        
        if (isClosing inputHead)
        then
            let expectedClosing = expectedRemaining |> List.tryHead
            match expectedClosing with
            | None -> Some inputHead, expectedRemaining
            | Some c ->
                if inputHead <> c
                then Some inputHead, expectedRemaining
                else solver inputTail (parsedLength + 1) (expectedRemaining |> List.tail)
        else
            solver inputTail (parsedLength + 1) (getClosingOf inputHead :: expectedRemaining)

let scoreError c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "Invalid character"

let solve1 input =
    input
    |> Seq.map (fun line -> solver line 0 [])
    |> Seq.filter (fun (invalid, _) -> invalid |> Option.isSome)
    |> Seq.map (fun (invalid, _) -> invalid.Value)
    |> Seq.map scoreError
    |> Seq.reduce ( + )

let scoreRemaining c =
    match c with
    | ')' -> 1UL
    | ']' -> 2UL
    | '}' -> 3UL
    | '>' -> 4UL
    | _ -> failwith "Invalid character"

let solve2 input =
    input
    |> Seq.map (fun line -> solver line 0 [])
    |> Seq.filter (fun (invalid, _) -> invalid |> Option.isNone)
    |> Seq.map (fun (_, remaining) -> remaining |> Seq.map scoreRemaining)
    |> Seq.map (fun line ->
        line |> Seq.fold (fun s c ->
            s * 5UL + c) 0UL)
    |> Seq.sort
    |> fun s -> s |> Seq.skip ((s |> Seq.length) / 2) |> Seq.head


solve1 testInput
solve2 testInput

solve1 puzzleInput
solve2 puzzleInput