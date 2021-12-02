open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("input.txt")

type Command =
    | Forward of int
    | Down of int
    | Up of int

type Position = {
    Distance: int;
    Depth: int;
}

let startingPosition = { Distance = 0; Depth = 0 }

let commandRegex = Regex "^(?<direction>(forward|down|up)) (?<amount>(\d+))$"

let stringToCommand input =
    let commandRegexMatch = commandRegex.Match input
    let direction = commandRegexMatch.Groups.["direction"].Value
    let amount = commandRegexMatch.Groups.["amount"].Value |> int

    match direction with
    | "forward" -> Forward amount
    | "up" -> Up amount
    | "down" -> Down amount
    | _ -> failwith "Invalid operation"

let commandFolder state command =
    match command with
    | Forward amount -> { state with Distance = state.Distance + amount }
    | Up amount -> { state with Depth = state.Depth - amount }
    | Down amount -> { state with Depth = state.Depth + amount }

input
|> Seq.map stringToCommand
|> Seq.fold commandFolder startingPosition
|> fun p -> p.Distance * p.Depth



type PositionWithAim = {
    Distance: int;
    Depth: int;
    Aim: int;
}

let startingPositionWithAim = {
    Distance = 0;
    Depth = 0;
    Aim = 0;
}

let commandFolder2 (state: PositionWithAim) (command: Command) =
    match command with
    | Forward amount -> { state with Distance = state.Distance + amount; Depth = state.Depth + (state.Aim * amount) }
    | Up amount -> { state with Aim = state.Aim - amount }
    | Down amount -> { state with Aim = state.Aim + amount }

input
|> Seq.map stringToCommand
|> Seq.fold commandFolder2 startingPositionWithAim
|> fun p -> p.Distance * p.Depth
