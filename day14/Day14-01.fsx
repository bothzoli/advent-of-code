open System.IO


let testFile = File.ReadLines "test.txt"
testFile

let realFile = File.ReadLines "input.txt"
realFile

type Instruction = Mask of BitMask: string | Assignment of Address: int * Number: string

let parseLine (line : string) =
    if line.StartsWith("mask")
    then Mask(line.Substring(7))
    else
        let address = line.Substring(4).Split("] = ", 2) |> Array.head |> int
        let number =
            line.Substring(4).Split("] = ", 2)
            |> Array.skip 1
            |> Array.head
            |> int64
            |> fun num -> System.Convert.ToString(num, 2).PadLeft(36, '0')
        Assignment(address, number)

let applyMask (mask : string) (num : string) =
    (mask, num)
    ||> Seq.zip
    |> Seq.map (fun (m, n) -> match m with '1' -> '1' | '0' -> '0' | 'X' -> n | _ -> failwith "Invalid mask")
    |> Seq.toArray
    |> System.String

applyMask "10X1101X10X111011001X00X0110101X11XX" "000000000000000110000001000010011100"

let foldLines (mask, result) entry =
    match entry with
    | Mask(m) -> (m, result)
    | Assignment(address, number) -> (mask, Assignment(address, applyMask mask number) :: result)


testFile
|> Seq.map parseLine
|> Seq.fold foldLines ("", [])
|> snd
|> List.distinctBy (fun e -> match e with Assignment(address, _) -> address | Mask(_) -> 0)
|> List.sumBy ((fun e -> match e with Assignment(_, num) -> num | Mask(_) -> "0") >> (fun n -> System.Convert.ToUInt64(n, 2)))

realFile
|> Seq.map parseLine
|> Seq.fold foldLines ("", [])
|> snd
|> List.distinctBy (fun e -> match e with Assignment(address, _) -> address | Mask(_) -> 0)
|> List.sumBy ((fun e -> match e with Assignment(_, num) -> num | Mask(_) -> "0") >> (fun n -> System.Convert.ToUInt64(n, 2)))
