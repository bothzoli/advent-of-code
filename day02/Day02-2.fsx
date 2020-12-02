open System.IO

let passwordPolicies = "input.txt" |> File.ReadAllLines |> Array.toList

type PasswordPolicy =
    {
        EqualsLocation: int
        DiffersLocation: int
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

    let (eqLoc, difLoc) = match (nums.Split('-')) with
    | [|eqLoc; difLoc|] -> (eqLoc |> int, difLoc |> int)
    | _ -> failwith "Invalid policy"

    {
        EqualsLocation = eqLoc
        DiffersLocation = difLoc
        Character = char
        Password = password
    }

let validatePolicy policy =
    let equalsChar = policy.Password.[policy.EqualsLocation] |> string
    let diffChar = policy.Password.[policy.DiffersLocation] |> string
    (equalsChar = policy.Character && diffChar <> policy.Character) || (equalsChar <> policy.Character && diffChar = policy.Character)

parsePolicy "3-4 p: bxptpp"
|> validatePolicy

parsePolicy "1-3 a: abcde"
|> validatePolicy

parsePolicy "1-3 b: cdefg"
|> validatePolicy

parsePolicy "2-9 c: ccccccccc"
|> validatePolicy


passwordPolicies
|> List.map (parsePolicy >> validatePolicy)
|> List.filter (id)
|> List.length