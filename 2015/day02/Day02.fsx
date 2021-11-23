open System.IO

type PresentBox = {
    Width: int;
    Length: int;
    Height: int;
}

let getRequiredWrappingPaper box =
    let sides = [box.Length * box.Width; box.Width * box.Height; box.Height * box.Length]
    (sides |> List.sum) * 2 + (sides |> List.min)

getRequiredWrappingPaper { Width = 2; Length = 3; Height = 4}
getRequiredWrappingPaper { Width = 1; Length = 1; Height = 10}

let input = File.ReadAllLines "input.txt"

let inputToBoxes (input: string array) =
    input
    |> Array.map (fun line -> line.Split('x') |> Array.map (int))
    |> Array.map (fun [|w; l; h|] -> { Width = w; Length = l; Height = h})

input
|> inputToBoxes
|> Array.map getRequiredWrappingPaper
|> Array.sum

let getRequiredRibbon box =
    let sides = [box.Length; box.Width; box.Height] |> List.sort
    (sides |> List.take 2 |> List.sum) * 2 + (sides |> List.fold (*) 1)

getRequiredRibbon { Width = 2; Length = 3; Height = 4}
getRequiredRibbon { Width = 1; Length = 1; Height = 10}

input
|> inputToBoxes
|> Array.map getRequiredRibbon
|> Array.sum
