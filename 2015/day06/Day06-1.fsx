open System.IO
open System.Text.RegularExpressions

type SwitchType = On | Off | Toggle

type Command = {
    Type: SwitchType;
    X: int;
    Y: int
}

let stringToSwitch input =
    match input with
    | "turn on" -> On
    | "turn off" -> Off
    | "toggle" -> Toggle
    | _ -> failwith "Invalid switch"

let getRegexMatchGroups input =
    let matcher = Regex("(?<lights>(turn off|turn on|toggle)) ((?<fromX>\d+),(?<fromY>\d+)) through ((?<toX>\d+),(?<toY>\d+))")
    matcher.Match(input).Groups

let inputLineToCommand input =
    let groups = getRegexMatchGroups input
    let fromX = groups.["fromX"].Value |> int
    let fromY = groups.["fromY"].Value |> int
    let toX = groups.["toX"].Value |> int
    let toY = groups.["toY"].Value |> int
    seq {
        for row in fromX .. toX do
            for column in fromY .. toY ->
            {
                Type = stringToSwitch(groups.["lights"].Value)
                X = row;
                Y = column
            }
    }

inputLineToCommand "turn off 3,9 through 4,12"
inputLineToCommand "turn on 3,9 through 4,12"
inputLineToCommand "toggle 3,9 through 4,12"

let input = File.ReadAllLines "input.txt"

type Position = {
    X: int;
    Y: int
}

let commandToPosition (command: Command) =
    {
        X = command.X;
        Y = command.Y
    }

let commandFolder (state: Set<Position>) current =
    let position = commandToPosition current
    match current.Type with
    | On -> state.Add(position)
    | Off -> state.Remove(position)
    | Toggle -> if state.Contains(position) then state.Remove(position) else state.Add(position)


input
|> Array.toSeq
|> Seq.map inputLineToCommand
|> Seq.concat
|> Seq.fold commandFolder Set.empty
|> Set.count

let result1 = 543903

type BrightnessLamps = Map<Position,int>

let commandFolder2 (state: BrightnessLamps) current =
    let position = commandToPosition current
    state |> Map.change position (fun x ->
        match x with
        | Some s ->
            match current.Type with
            | On -> Some (s + 1)
            | Toggle -> Some (s + 2)
            | Off -> Some ((max (s - 1) 0))
        | None ->
            match current.Type with
            | On -> Some (1)
            | Toggle -> Some (2)
            | Off -> None
        )

input
|> Array.toSeq
|> Seq.map inputLineToCommand
|> Seq.concat
|> Seq.fold commandFolder2 Map.empty
|> Map.fold (fun state _ brightness -> state + (brightness |> uint64)) 0UL

let result2 = 14687245
