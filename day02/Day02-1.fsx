open System.IO

let passwordPolicies = "input.txt" |> File.ReadAllLines |> Array.toList

type PasswordPolicy =
    {
        MinNumber: int
        MaxNumber: int
        Character: string
        Password: string
    }

"3-4 p: bxptpp".Split([|'-'; ' '; ':'|])

let parsePolicy (policy:string) =
    let (minNum, maxNum, char, password) =
        match (policy.Split([|'-'; ' '; ':'|])) with
        | [|minNum; maxNum; char; _; password|] -> (minNum |> int, maxNum |> int, char, password)
        | _ -> failwith "Invalid policy"

    {
        MinNumber = minNum
        MaxNumber = maxNum
        Character = char
        Password = password
    }

parsePolicy "3-4 p: bxptpp"

"asdfqwer" |> Seq.filter(fun c -> c = 'f') |> Seq.length

let validatePolicy policy =
    let numberOfChars = policy.Password |> Seq.filter(fun c -> string(c) = policy.Character) |> Seq.length
    numberOfChars >= policy.MinNumber && numberOfChars <= policy.MaxNumber

parsePolicy "3-4 p: bxptpp"
|> validatePolicy


passwordPolicies
|> List.map (parsePolicy >> validatePolicy)
|> List.filter (id)
|> List.length
