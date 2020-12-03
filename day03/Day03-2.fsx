open System.IO

let linesFromFile filepath =
    seq { use reader = File.OpenText filepath
          while not reader.EndOfStream
             do yield reader.ReadLine () }

let lineLength input = input |> (Seq.head >> Seq.length)

let indices input right down = 
    let length = lineLength input
    Seq.initInfinite (fun index ->
        if index % down = 0 then (index / down) * right % length else -1)

let getCharAtPosition (textLine:string, index) =
    try
        textLine.[index]
    with :? System.IndexOutOfRangeException -> '.'

let isTree chr = chr = '#'

let getTreesForSlope track rule =
    let right, down = rule
    (track, ((indices track right down)))
    ||> Seq.zip
    |> Seq.map getCharAtPosition
    |> Seq.filter isTree
    |> Seq.length

let rules = seq {(1,1); (3, 1); (5, 1); (7, 1); (1, 2)}

let getTreesForTestSlope = getTreesForSlope (linesFromFile "test.txt")
let getTreesForRealSlope = getTreesForSlope (linesFromFile "input.txt")


rules
|> Seq.map (getTreesForTestSlope >> int64)
|> Seq.reduce (*)


rules
|> Seq.map (getTreesForRealSlope >> int64)
|> Seq.reduce (*)
