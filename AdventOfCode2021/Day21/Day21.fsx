open System.IO

let testInput = File.ReadAllLines("day21/test.txt")

let inputToStartingPositions (input: string seq) =
    input
    |> Seq.map (fun line ->
        line.Split(' ')
        |> Seq.last
        |> int)
    |> (fun s ->
        let player1 = s |> Seq.head
        let player2 = s |> Seq.last

        (player1, player2))

let dieSequence =
    Seq.initInfinite (fun index -> index % 10) |> Seq.skip 1

let rollDie sequence =
    let nextRoll = sequence |> Seq.take 3 |> Seq.reduce (+) |> fun roll -> roll % 10
    nextRoll, sequence |> Seq.skip 3

let rec movePlayer (player1Position, player2Position, player1Score, player2Score, rolls, rollCount) =
    let scorePosition position =
        match position with
        | 0 -> 10
        | p -> p

    let (player1Roll, remainingRolls) = rollDie rolls
    let newPlayer1Position = (player1Position + player1Roll) % 10
    let newPlayer1Score = player1Score + scorePosition newPlayer1Position
    let (player2Roll, remainingRolls) = rollDie remainingRolls
    let newPlayer2Position = (player2Position + player2Roll) % 10
    let newPlayer2Score = player2Score + scorePosition newPlayer2Position

    if (newPlayer1Score >= 1000)
    then
        (newPlayer1Position, player2Position, newPlayer1Score, player2Score, remainingRolls, rollCount + 3)
    elif (newPlayer2Score >= 1000)
    then
        (newPlayer1Position, newPlayer2Position, newPlayer1Score, newPlayer2Score, remainingRolls, rollCount + 6)
    else
        movePlayer (newPlayer1Position, newPlayer2Position, newPlayer1Score, newPlayer2Score, remainingRolls, rollCount + 6)

movePlayer (4, 8, 0, 0, dieSequence, 0)
|> (fun (_, _, _, sc2, _, roll) -> sc2 * roll)

movePlayer (10, 8, 0, 0, dieSequence, 0)
|> (fun (_, _, _, sc2, _, roll) -> sc2 * roll)
