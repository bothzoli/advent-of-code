open System.IO

let linesFromFile filepath =
    seq { use reader = File.OpenText filepath
          while not reader.EndOfStream
             do yield reader.ReadLine () }

let testInput = linesFromFile "test.txt"

let lineLength input = input |> (Seq.head >> Seq.length)

let indices lineLength = 
    Seq.initInfinite (fun index ->
        index * 3 % lineLength)

let testIndices = indices (lineLength testInput)

testIndices
|> Seq.take 10
|> Seq.toList

let getCharAtPosition (textLine:string, index) =
    textLine.[index]

let isTree char =
    char = '#'

(testInput, testIndices)
||> Seq.zip
|> Seq.map getCharAtPosition
|> Seq.filter isTree
|> Seq.length


let realInput = linesFromFile "input.txt"

let realIndices = indices (lineLength realInput)

(realInput, realIndices)
||> Seq.zip
|> Seq.map getCharAtPosition
|> Seq.filter isTree
|> Seq.length
