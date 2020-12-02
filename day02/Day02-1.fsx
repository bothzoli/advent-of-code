open System.IO

let passwordPolicies = "input.txt" |> File.ReadAllLines |> Array.toList

type PasswordPolicy =
    {
        MinNumber: int
        MaxNumber: int
        Character: string
        Password: string
    }

"asdf:qwer".Split(':')

let parsePolicy (policy:string) =
    let (rules, password) = match (policy.Split(':')) with
    | [|rules; password|] -> (rules, password)
    | _ -> failwith "Invalid policy"

    let (nums, char) = match (rules.Split(' ')) with
    | [|nums; char|] -> (nums, char)
    | _ -> failwith "Invalid policy"

    let (minNum, maxNum) = match (nums.Split('-')) with
    | [|minNum; maxNum|] -> (minNum |> int, maxNum |> int)
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