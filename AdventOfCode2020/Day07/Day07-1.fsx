open System.IO
open System.Text.RegularExpressions

let sampleLine1 = "light red bags contain 1 bright white bag, 2 muted yellow bags."
let sampleLine2 = "light red bags contain 2 muted yellow bags."
let sampleLine3 = "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags, 12 muted yellow bags."
let sampleLine4 = "dotted black bags contain no other bags."

let getRootNode (input:string) =
    let [color1; color2] = input.Split(' ') |> Array.toList |> List.take 2
    color1 + " " + color2

getRootNode sampleLine1
getRootNode sampleLine2
getRootNode sampleLine3
getRootNode sampleLine4

let bagRegex = Regex @"(\d+ \w+ \w+)+"

let destructureNode (input:string) =
    let [| weight; color |] = input.Split(' ', 2)
    (weight |> int, color)

let getListOfAdjacentNodes (input:string) =
    input
    |> bagRegex.Matches
    |> Seq.map ((fun m -> m.Value) >> destructureNode)

getListOfAdjacentNodes sampleLine1
getListOfAdjacentNodes sampleLine2
getListOfAdjacentNodes sampleLine3
getListOfAdjacentNodes sampleLine4

let testFile = File.ReadLines "test.txt"

getRootNode sampleLine1
getListOfAdjacentNodes sampleLine1

let changeNodeDirection rootNode listOfAdjacentNodes =
    listOfAdjacentNodes
    |> Seq.map (fun (weight, node) -> (node, [(weight, rootNode)]))

let getEdges input =
    changeNodeDirection (getRootNode input) (getListOfAdjacentNodes input)

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


let adjacencyList = testFile |> Seq.map getEdges |> Seq.fold (Seq.append) Seq.empty |> Seq.fold mapFolder Map.empty


adjacencyList.["shiny gold"]
|> List.fold (fun s (w, n) -> s + w) 0

let getAdjacentNodes listEntry =
    listEntry |> List.map snd |> Set.ofList

let rec getAllNeighbours adjList node =
    if adjList |> Map.containsKey node
    then 
        Set.union (adjList.[node] |> getAdjacentNodes) (adjList.[node] |> List.fold (fun s (_, n) -> Set.union s (getAllNeighbours adjList n)) Set.empty)
    else Set.empty

getAllNeighbours adjacencyList "shiny gold"
|> Set.count

let realAdjacencyList = (File.ReadLines "input.txt") |> Seq.map getEdges |> Seq.fold (Seq.append) Seq.empty |> Seq.fold mapFolder Map.empty

getAllNeighbours realAdjacencyList "shiny gold"
|> Set.count
