type SpokenNumber = {
    LastSpoken: int
    SpokenBefore: int option
}

let testInput0 = [0; 3; 6]
let testInput1 = [1; 3; 2]

let createInitialMap inputList =
    inputList
    |> List.mapi (fun index num -> num, { LastSpoken = index + 1; SpokenBefore = None })
    |> Map.ofList


let testInitialMap1 = createInitialMap testInput1


testInitialMap1.TryFind 1
testInitialMap1.TryFind 4

let mapChanger roundNumber mapEntry =
    match mapEntry with
    | Some(entry) -> Some({ LastSpoken = roundNumber; SpokenBefore = Some(entry.LastSpoken) })
    | None -> Some({ LastSpoken = roundNumber; SpokenBefore = None })

testInitialMap1 |> Map.find 3
testInitialMap1 |> Map.change 1 (mapChanger 4)

let sequenceFolder (previousNumber, numberMap) roundNumber =
    let previousSpokenNumber = numberMap |> Map.find previousNumber

    let nextNumber =
        match previousSpokenNumber.SpokenBefore with
        | None -> 0
        | Some(spokenBefore) -> previousSpokenNumber.LastSpoken - spokenBefore

    (nextNumber, numberMap |> Map.change nextNumber (mapChanger roundNumber))

let getNthResult (inputList: int list) n =
    [inputList.Length + 1 .. n]
    |> List.fold sequenceFolder (inputList.[inputList.Length - 1], createInitialMap inputList)
    |> fst


getNthResult testInput0 2020

getNthResult [1; 3; 2] 2020
getNthResult [2; 1; 3] 2020

getNthResult [16; 12; 1; 0; 15; 7; 11] 2020

getNthResult [1; 3; 2] 30000000

getNthResult [16; 12; 1; 0; 15; 7; 11] 30000000
