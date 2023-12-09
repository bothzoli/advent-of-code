open System.IO

let testInput = File.ReadAllLines("./AdventOfCode2023/Day02/test.txt")
let puzzleInput = File.ReadAllLines("./AdventOfCode2023/Day02/input.txt")

type Color =
    | Red
    | Green
    | Blue

type Pull = { Count: int; Color: Color }

type Take = Pull seq

type Game = { GameId: int; Takes: Take seq }

type GameValue = { Red: int; Green: int; Blue: int }

type EvaluatedGame =
    { GameId: int
      GameValues: GameValue seq }

type MaxCountGame =
    { GameId: int
      GameValue: GameValue }

type PreCondition = { Red: int; Green: int; Blue: int }

let lineToEvaluatedGame (input: string) : EvaluatedGame =
    let parseColor input =
        match input with
        | "red" -> Red
        | "green" -> Green
        | "blue" -> Blue
        | _ -> failwith "Invalid color"

    let lineToGameId (input: string) : int =
        input
        |> Seq.skip 5
        |> Seq.takeWhile (fun x -> x <> ':')
        |> Seq.map string
        |> Seq.reduce (+)
        |> int

    let lineToGames (input: string) : string =
        input
        |> Seq.skipWhile (fun x -> x <> ':')
        |> Seq.skip 2
        |> Seq.map string
        |> String.concat ""

    let gameToTakes (input: string) =
        input.Split "; " |> Seq.map (fun g -> g.Split ", ") |> Seq.map Array.toSeq

    let takeToPull (input: string) : Pull =
        input.Split " "
        |> fun x ->
            { Count = x |> Seq.head |> int
              Color = x |> Seq.skip 1 |> Seq.head |> parseColor }

    let sumTake (take: Take) : Take =
        take
        |> Seq.groupBy (_.Color)
        |> Seq.map (fun x ->
            { Count = (snd x) |> Seq.sumBy (_.Count)
              Color = fst x })

    let rec evaluateGame (game: Game) : EvaluatedGame =
        let takeToGameValue (take: Take) : GameValue =
            { Red =
                match take |> Seq.filter (fun t -> t.Color = Color.Red) |> Seq.tryHead with
                | Some x -> x.Count
                | None -> 0
              Green =
                match take |> Seq.filter (fun t -> t.Color = Color.Green) |> Seq.tryHead with
                | Some x -> x.Count
                | None -> 0
              Blue =
                match take |> Seq.filter (fun t -> t.Color = Color.Blue) |> Seq.tryHead with
                | Some x -> x.Count
                | None -> 0 }

        { GameId = game.GameId
          GameValues = game.Takes |> Seq.map takeToGameValue }


    { GameId = input |> lineToGameId
      Takes =
        input
        |> lineToGames
        |> gameToTakes
        |> Seq.map (Seq.map takeToPull)
        |> Seq.map sumTake }
    |> evaluateGame

let solve1 condition input =
    let evaluatedGameFulfillsPrecondition (preCondition: PreCondition) (evaluatedGame: EvaluatedGame) : bool =
        evaluatedGame.GameValues
        |> Seq.forall (fun gameValues ->
            gameValues.Red <= preCondition.Red
            && gameValues.Green <= preCondition.Green
            && gameValues.Blue <= preCondition.Blue)

    input
    |> Seq.map lineToEvaluatedGame
    |> Seq.filter (evaluatedGameFulfillsPrecondition condition)
    |> Seq.sumBy (_.GameId)

let preCondition = { Red = 12; Green = 13; Blue = 14 }

let solve2 input =
    let getHighestCountForEachColor (gameValues: GameValue seq): GameValue =
        { Red = (gameValues |> Seq.maxBy (_.Red)).Red
          Green = (gameValues |> Seq.maxBy (_.Green)).Green
          Blue = (gameValues |> Seq.maxBy (_.Blue)).Blue }

    input
    |> Seq.map lineToEvaluatedGame
    |> Seq.map (fun g -> getHighestCountForEachColor g.GameValues)
    |> Seq.map (fun g -> g.Red * g.Green * g.Blue)
    |> Seq.sum

solve1 preCondition testInput
solve2 testInput

solve1 preCondition puzzleInput
solve2 puzzleInput