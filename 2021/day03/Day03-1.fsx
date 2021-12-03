open System
open System.IO

let input = File.ReadAllLines("input.txt")

let stringToIntArray (input: string) =
    input
    |> Seq.map (fun x -> (x |> int) - 48)
    |> Seq.toArray

let intArrayAdd arr1 arr2 = Array.map2 (+) arr1 arr2

let getGammaAndEpsilon (input: string array) =
    let cutoff = input.Length / 2

    input
    |> Seq.map stringToIntArray
    |> Seq.reduce intArrayAdd
    |> Array.map (fun x -> if x > cutoff then ("1", "0") else ("0", "1"))
    |> Array.reduce (fun (gs, es) (g, e) -> (gs + g, es + e))
    |> fun (g, e) -> (Convert.ToInt32(g, 2), Convert.ToInt32(e, 2))

getGammaAndEpsilon input |> fun (g, e) -> g * e
