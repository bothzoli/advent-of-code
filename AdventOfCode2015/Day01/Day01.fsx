open System.IO

let input = "input.txt" |> File.ReadAllLines |> Seq.ofArray |> Seq.head

let test1 = "(())"

let parensMap parens =
    match parens with
    | '(' -> 1
    | ')' -> -1
    | _ -> failwith "Incorrect input"

test1
|> Seq.map parensMap
|> Seq.sum

input
|> Seq.map parensMap
|> Seq.sum

let parensFolder (result, state) (i, parens) =
    if result > 0 then (result, state)
    else if (state + parens = -1) then (i + 1, state + parens)
    else (0, state + parens)

")"
|> Seq.mapi (fun i x -> i, parensMap x)
|> Seq.fold parensFolder (0, 0)

"()())"
|> Seq.mapi (fun i x -> i, parensMap x)
|> Seq.fold parensFolder (0, 0)

input
|> Seq.mapi (fun i x -> i, parensMap x)
|> Seq.fold parensFolder (0, 0)
