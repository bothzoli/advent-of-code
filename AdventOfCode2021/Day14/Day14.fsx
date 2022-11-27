open System.IO

let testInput = File.ReadAllLines("2021/day14/test.txt")
let puzzleInput = File.ReadAllLines("2021/day14/input.txt")

let solve forIteration input =
    let splitSubstitutionRoleString (input: string) =
        let [|from; newComponent|] = input.Split(" -> ")
        let left = from |> Seq.head |> string
        let right = from |> Seq.last |> string
        (from, left, right, newComponent)

    let getSubstitutionRules input =
        input 
        |> Seq.skip 2
        |> Seq.map (fun i ->
            let (from, left, right, newComponent) = splitSubstitutionRoleString i
            from, [|left + newComponent; newComponent + right|])
        |> Map.ofSeq

    let stringToCountMap input =
        input
        |> Seq.countBy (id)
        |> Seq.map (fun (c, i) -> (c, uint64 i))
        |> Map.ofSeq

    let getComponentCounts input =
        input
        |> Seq.skip 2
        |> Seq.map (fun i ->
            let (from, left, right, newComponent) = splitSubstitutionRoleString i
            let componentCount = left + newComponent + newComponent + right |> stringToCountMap
            ((from, 1), componentCount))
        |> Map.ofSeq

    let addComponentCounts aMap bMap =
        aMap
        |> Map.fold (fun b aKey aValue ->
            let aInB = b |> Map.tryFind aKey
            match aInB with
            | Some(bValue) -> b |> Map.add aKey (aValue + bValue)
            | None -> b |> Map.add aKey aValue
        ) bMap

    let getFirstAndLast input = (input |> Seq.head |> string) + (input |> Seq.last |> string)

    let getPolymerString input = input |> Seq.head

    let iterateComponentCounts iteration input =
        let substitutionRules = getSubstitutionRules input
        let componentCounts = getComponentCounts input

        let rec iterate count iteration substitutionRules componentCounts =
            if count >= iteration
            then componentCounts
            else
                let newComponentCounts =
                    componentCounts
                    |> Map.keys
                    |> Seq.map fst
                    |> Seq.fold (fun s c ->
                        let [|left; right|] = substitutionRules |> Map.find c
                        let leftComponentCounts = s |> Map.find (left, count + 1)
                        let rightComponentCounts = s |> Map.find (right, count + 1)
                        s |> Map.add (c, count + 2) (addComponentCounts leftComponentCounts rightComponentCounts)
                    ) componentCounts
                iterate (count + 1) iteration substitutionRules newComponentCounts

        iterate 0 (iteration - 1) substitutionRules componentCounts

    let explodePolymer (input: string) =
        input
        |> Seq.windowed 2
        |> Seq.map (fun a -> a |> Seq.map string |> Seq.reduce (+))

    let componentCountMap = iterateComponentCounts forIteration input
    
    getPolymerString input
    |> getFirstAndLast
    |> stringToCountMap
    |> addComponentCounts
        (explodePolymer (getPolymerString input)
        |> Seq.map (fun p -> componentCountMap |> Map.find (p, forIteration))
        |> Seq.reduce addComponentCounts
        |> Map.map (fun k v -> v / 2UL))
    |> Map.toList
    |> List.sortByDescending snd
    |> fun l ->
        (l |> List.head |> snd) - (l |> List.last |> snd)

solve 10 testInput
solve 40 testInput

solve 10 puzzleInput
solve 40 puzzleInput
