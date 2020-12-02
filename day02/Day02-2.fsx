open System.IO

let passwordPolicies = "input.txt" |> File.ReadAllLines |> Array.toSeq

type PasswordPolicy =
    {
        EqualsLocation: int
        DiffersLocation: int
        Character: string
        Password: string
    }

let parsePolicy (policy:string) =
    let (eqLoc, difLoc, char, password) =
        match (policy.Split([|'-'; ' '; ':'|])) with
        | [|eqLoc; difLoc; char; _; password|] -> (eqLoc |> int, difLoc |> int, char, password)
        | _ -> failwith "Invalid policy"

    {
        EqualsLocation = eqLoc
        DiffersLocation = difLoc
        Character = char
        Password = password
    }

let validatePolicy policy =
    let equalsChar = policy.Password.[policy.EqualsLocation - 1] |> string
    let diffChar = policy.Password.[policy.DiffersLocation - 1] |> string
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
|> Seq.map (parsePolicy >> validatePolicy)
|> Seq.filter (id)
|> Seq.length
