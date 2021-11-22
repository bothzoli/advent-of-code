open System
open System.IO


let testFile = File.ReadLines "test_alt.txt"
testFile

let realFile = File.ReadLines "input.txt"
realFile

type Instruction = Mask of BitMask: string | Assignment of Address: string * Number: int64

let parseLine (line : string) =
    if line.StartsWith("mask")
    then Mask(line.Substring(7))
    else
        let address =
            line.Substring(4).Split("] = ", 2)
            |> Array.head
            |> int64
            |> fun num -> System.Convert.ToString(num, 2).PadLeft(36, '0')
        let number =
            line.Substring(4).Split("] = ", 2)
            |> Array.skip 1
            |> Array.head
            |> int64
        Assignment(address, number)


let collectChar chr prefixes =
    match chr with
    | 'X' ->
        prefixes
        |> List.collect (fun prefix ->
        ['1'; '0']
        |> List.map (fun s -> prefix + string s))
    | c ->
        prefixes
        |> List.map (fun prefix -> prefix + string c)

collectChar '1' [""]
|> collectChar 'X'

collectChar '1' ["0001"; "1001"]
collectChar '0' ["0001"; "1001"]
collectChar 'X' ["0001"; "1001"]

let floatingAddressFolder state entry =
    collectChar entry state

let applyMask (mask : string) (address : string) =
    let floatingAddress =
        (mask, address)
        ||> Seq.zip
        |> Seq.map (fun (m, n) ->
            match m with
            | '1' -> '1'
            | '0' -> n
            | 'X' -> 'X'
            | _ -> failwith "Invalid mask")
        |> Seq.toArray
        |> String
    floatingAddress
    |> Seq.fold floatingAddressFolder [""]

applyMask "00000000000000000000000000000XX1001X" "000000000000000000000000000000101010"

let foldLines (mask, result) entry =
    match entry with
    | Mask(m) -> (m, result)
    | Assignment(address, number) ->
        let newAssignments =
            applyMask mask address
            |> List.map (fun a -> (Assignment(a, number)))
        (mask, newAssignments @ result)

testFile
|> Seq.map parseLine
|> Seq.fold foldLines ("", [])
|> snd
|> List.distinctBy (fun e -> match e with Assignment(address, _) -> address | Mask(_) -> "")
|> List.sumBy (fun e -> match e with Assignment(_, num) -> num | Mask(_) -> 0L)

realFile
|> Seq.map parseLine
|> Seq.fold foldLines ("", [])
|> snd
|> List.distinctBy (fun e -> match e with Assignment(address, _) -> address | Mask(_) -> "")
|> List.sumBy (fun e -> match e with Assignment(_, num) -> num | Mask(_) -> 0L)
