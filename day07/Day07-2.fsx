open System.IO
open System.Text.RegularExpressions

let getRootNode (input:string) =
    let [color1; color2] = input.Split(' ') |> Array.toList |> List.take 2
    color1 + " " + color2

let bagRegex = Regex @"(\d+ \w+ \w+)+"

let destructureNode (input:string) =
    let [| weight; color |] = input.Split(' ', 2)
    (weight |> int, color)

let getListOfAdjacentNodes (input:string) =
    input
    |> bagRegex.Matches
    |> Seq.map ((fun m -> m.Value) >> destructureNode)
    |> Seq.toList

let testFile = File.ReadLines "test.txt"

let getEdges input = (getRootNode input), (getListOfAdjacentNodes input)

let appendToListOption newEntries inputList =
    match inputList with
    | Some(l) -> List.append l newEntries |> Some
    | None -> newEntries |> Some

let appendToMap originalMap key listOfEntries =
    originalMap |> Map.change key (appendToListOption listOfEntries)

let mapFolder mapState newEntry =
    let (startNode, adjacencyList) = newEntry
    if not (mapState |> Map.containsKey startNode)
    then mapState.Add(startNode, adjacencyList)
    else (appendToMap mapState startNode adjacencyList)


let adjacencyList = testFile |> Seq.map getEdges |> Seq.fold mapFolder Map.empty


adjacencyList.["shiny gold"]
|> List.fold (fun s (w, n) -> s + w) 1

let rec getAllWithin adjList node =
    if adjList |> Map.containsKey node
    then (adjList.[node] |> List.fold (fun s (w, n) -> s + w * (getAllWithin adjList n)) 0) + 1
    else 1

(getAllWithin adjacencyList "shiny gold") - 1

let realAdjacencyList = (File.ReadLines "input.txt") |> Seq.map getEdges |> Seq.fold mapFolder Map.empty

(getAllWithin realAdjacencyList "shiny gold") - 1
