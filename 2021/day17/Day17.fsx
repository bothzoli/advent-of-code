open System.IO

let testInput = "target area: x=20..30, y=-10..-5"
let puzzleInput = File.ReadAllText("day17/input.txt")

let stringToTargetZone (input: string) =
    let ((x0, x1), (y1, y0)) =
        input.Split(' ')
        |> Seq.skip 2
        |> Seq.map (
            (fun s -> s.Split('=') |> Seq.tail |> Seq.head) >>
            (fun s -> s.Trim(',').Split("..")) >>
            (Seq.map int) >>
            (Seq.sort) >>
            (fun s -> s |> Seq.head, s |> Seq.tail |> Seq.head)
        ) |> (fun s -> s |> Seq.head, s |> Seq.tail |> Seq.head)
    (x0, x1), (y0, y1)


let origin = 0, 0

let isInTargetZone ((x0, x1), (y0, y1)) (x, y) =
    if ((x0 <= x) && (x <= x1)) && ((y0 >= y) && (y >= y1))
    then true
    else false

let rec willReachTargetZone targetZone pos vel maxY =
    let doStep (px, py) (vx, vy) =
        let pos' = px + vx, py + vy
        let vel' = (if vx > 0 then vx - 1 elif vx < 0 then vx + 1 else vx), vy - 1

        pos', vel'
    
    let (px, py) = pos
    let (vx, _) = vel
    let (tx0, tx1), (_, ty1) = targetZone

    let maxY' = if py > maxY then py else maxY

    if isInTargetZone targetZone pos
    then true, maxY'
    elif (px > tx1 && vx >= 0) || (px < tx0 && vx <= 0) then false, maxY'
    elif py < ty1 then false, maxY'
    else
        let (pos', vel') = doStep pos vel
        willReachTargetZone targetZone pos' vel' maxY'


let getMaxTargetZoneDistance ((tx0, tx1), (ty0, ty1)) = [ tx0; tx1; ty0; ty1 ] |> List.map abs |> List.max

let solver solverFn input =
    let targetZone = (stringToTargetZone input)
    let maxDistance = getMaxTargetZoneDistance targetZone
    {-maxDistance .. maxDistance}
    |> Seq.allPairs {0 .. maxDistance}
    |> Seq.map (fun velocity -> (willReachTargetZone targetZone origin velocity 0))
    |> Seq.filter fst
    |> Seq.map snd
    |> solverFn

let solve1 = solver Seq.max

solve1 testInput
solve1 puzzleInput

let solve2 = solver Seq.length

solve2 testInput
solve2 puzzleInput