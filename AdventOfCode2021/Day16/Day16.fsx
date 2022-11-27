open System.IO

let puzzleInput = File.ReadAllText("day16/input.txt")

let inputToBinaryStream (input: string) =
    let hexToInt (input: string) = System.Convert.ToInt32(input, 16)
    let intToPaddedBin (input: int) = System.Convert.ToString(input, 2).PadLeft(4, '0')

    input
    |> Seq.map (string >> hexToInt >> intToPaddedBin)
    |> Seq.reduce (+)


type Header = {
    Version: int;
    TypeId: int;
}

type Packet = LiteralPacket of Header * uint64 | OperatorPacket of Header * Packet list


let rec parsePacket input =
    let charSeqToString (input: char seq) = input |> Seq.map string |> Seq.reduce (+)
    let charSeqToInt =
        let parseBin input = System.Convert.ToInt32(input, 2)
        charSeqToString >> parseBin

    let parseHeader input =
        let takeFirstNCharacters n input =
            let firstN = input |> Seq.take n
            let remaining = input |> Seq.skip n
            firstN, remaining

        let (versionId, remaining) = takeFirstNCharacters 3 input
        let (typeId, payload) = takeFirstNCharacters 3 remaining

        {
            Version = (versionId |> charSeqToInt)
            TypeId = (typeId |> charSeqToInt)
        }, payload

    let getLiteralPayload input =
        let parseBinToUint64 input = System.Convert.ToUInt64(input, 2)
        let startsWithOne (chars: char seq) = chars |> Seq.head = '1'
        let unpackValue (chars: char array seq) = chars |> Seq.map Seq.tail |> Seq.map charSeqToString
        let inputChunks = input |> Seq.chunkBySize 5

        let beginning = inputChunks |> Seq.takeWhile startsWithOne |> unpackValue
        let last = inputChunks |> Seq.skipWhile startsWithOne |> Seq.take 1 |> unpackValue
        let literalValue = Seq.append beginning last |> Seq.reduce (+) |> parseBinToUint64

        let remaining = inputChunks |> Seq.skipWhile startsWithOne |> Seq.tail |> Seq.concat
        (literalValue, remaining)

    let parseSubPackets input =
        let lengthTypeId = input |> Seq.head

        if lengthTypeId = '0'
        then
            let length = input |> Seq.skip 1 |> Seq.take 15 |> charSeqToInt
            let toParse = input |> Seq.skip 16 |> Seq.take length
            let remaining = input |> Seq.skip 16 |> Seq.skip length

            let rec recParseSubPackets input packets =
                if input |> Seq.isEmpty
                then packets, Seq.empty<char>
                else
                    let (newPacket, remaining) = input |> parsePacket
                    recParseSubPackets remaining (newPacket :: packets)

            let (packetsRev, _) = recParseSubPackets toParse []
            packetsRev |> List.rev, remaining
        else
            let packetCount = input |> Seq.skip 1 |> Seq.take 11 |> charSeqToInt
            let toParse = input |> Seq.skip 12

            let rec recParseSubPackets input packets maxPacketCount currentPacketCount =
                if currentPacketCount = maxPacketCount
                then packets, input
                else
                    let (newPacket, remaining) = input |> parsePacket
                    recParseSubPackets remaining (newPacket :: packets) maxPacketCount (currentPacketCount + 1)

            let (packetsRev, remaining) = recParseSubPackets toParse [] packetCount 0
            packetsRev |> List.rev, remaining

    let (header, payload) = parseHeader input

    if header.TypeId = 4
    then
        let (literalValue, remaining) = getLiteralPayload payload
        LiteralPacket(header, literalValue), remaining
    else
        let (packetList, remaining) = parseSubPackets payload
        OperatorPacket(header, packetList), remaining

let rec getVersionSum packet =
    match packet with
    | LiteralPacket(header, _) -> header.Version
    | OperatorPacket(header, packets) -> header.Version + (packets |> List.map getVersionSum |> List.sum)


puzzleInput |> inputToBinaryStream |> parsePacket |> fst |> getVersionSum


let rec evaluatePacket packet =
    match packet with
    | LiteralPacket(_, literalValue) -> literalValue
    | OperatorPacket(header, packets) ->
        let packetValues = packets |> Seq.map evaluatePacket

        match header.TypeId with
        | 0 -> packetValues |> Seq.sum
        | 1 -> packetValues |> Seq.reduce ( * )
        | 2 -> packetValues |> Seq.min
        | 3 -> packetValues |> Seq.max
        | t ->
            let first = packetValues |> Seq.head
            let second = packetValues |> Seq.tail |> Seq.head

            match t with
            | 5 -> if first > second then 1UL else 0UL
            | 6 -> if first < second then 1UL else 0UL
            | 7 ->  if first = second then 1UL else 0UL
            | _ -> failwith "Invalid type ID"


puzzleInput |> inputToBinaryStream |> parsePacket |> fst |> evaluatePacket