open System.IO

let puzzleInput = File.ReadAllLines("2021/day08/input.txt")
let testInput = File.ReadAllLines("2021/day08/test.txt")

type Segment = A | B | C | D | E | F | G

type Number = Set<Segment>

let stringToNumber (input: string) =
    let charToSegment char =
        match char with
        | 'a' -> A
        | 'b' -> B
        | 'c' -> C
        | 'd' -> D
        | 'e' -> E
        | 'f' -> F
        | 'g' -> G
        | _ -> failwith "Invalid character"

    input.ToCharArray()
    |> Seq.map charToSegment
    |> Set.ofSeq


let getTranslateMap (input: Number seq) =
    let translateMap = Map.empty<Number, int>

    let one = input |> Seq.filter (fun s -> s.Count = 2) |> Seq.head
    let seven = input |> Seq.filter (fun s -> s.Count = 3) |> Seq.head
    let four = input |> Seq.filter (fun s -> s.Count = 4) |> Seq.head
    let eight = input |> Seq.filter (fun s -> s.Count = 7) |> Seq.head

    let two_three_five = input |> Seq.filter (fun s -> s.Count = 5)
    let six_nine_zero = input |> Seq.filter (fun s -> s.Count = 6)

    let nine =
        six_nine_zero
        |> Seq.filter (fun n ->
            n |> Set.union four = n && n |> Set.union seven = n) |> Seq.head
    
    let zero =
        six_nine_zero
        |> Seq.filter (fun n ->
            n |> Set.union four <> n && n |> Set.union seven = n) |> Seq.head

    let six = six_nine_zero |> Seq.filter (fun n -> n <> nine && n <> zero) |> Seq.head

    let three =
        two_three_five
        |> Seq.filter (fun n -> n |> Set.union seven = n) |> Seq.head

    let five =
        two_three_five
        |> Seq.filter (fun n ->
            n |> Set.union nine = nine && n |> Set.union seven = nine) |> Seq.head

    let two = two_three_five |> Seq.filter (fun n -> n <> three && n <> five) |> Seq.head

    translateMap
    |> Map.add one 1
    |> Map.add two 2
    |> Map.add three 3
    |> Map.add four 4
    |> Map.add five 5
    |> Map.add six 6
    |> Map.add seven 7
    |> Map.add eight 8
    |> Map.add nine 9
    |> Map.add zero 0

let lineToNumbersAndPuzzle (input: string) =
    let translateMap =
        input.Split('|')
        |> Seq.head
        |> fun s -> s.Trim().Split(' ')
        |> Seq.map stringToNumber
        |> fun s -> getTranslateMap s
    
    input.Split('|')
    |> Seq.last
    |> fun s -> s.Trim().Split(' ')
    |> Seq.map stringToNumber
    |> Seq.map (fun n -> translateMap |> Map.find n)
    |> Seq.map string
    |> Seq.reduce (+)
    |> (int)

testInput
|> Array.map lineToNumbersAndPuzzle
|> Array.sum

puzzleInput
|> Array.map lineToNumbersAndPuzzle
|> Array.sum
