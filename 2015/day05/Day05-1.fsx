open System.IO
open System.Text.RegularExpressions

let countVowelFolder = fun s c ->
    match c with
    | 'a' | 'e' | 'i' | 'o' | 'u' -> s + 1
    | _ -> s

countVowelFolder 0 'a'

"asdfeiua" |> Seq.fold countVowelFolder 0

let countainsAtLeastThreeVowels input =
    input |> Seq.fold countVowelFolder 0 >= 3

countainsAtLeastThreeVowels "asdfeiua"

let findDuplicateFolder = fun (success, previousChar) currentChar ->
    if success then (success, previousChar)
    else
        if currentChar = previousChar then (true, currentChar)
        else (false, currentChar)

"asdfeiua" |> Seq.fold findDuplicateFolder (false, '_')
"asddfeiua" |> Seq.fold findDuplicateFolder (false, '_')
"asddfeiiua" |> Seq.fold findDuplicateFolder (false, '_')

let containsDuplicateCharacter input =
    fst (input |> Seq.fold findDuplicateFolder (false, '_'))

containsDuplicateCharacter "asddfeiua"

let doesNotContainNotAllowed input =
    not <| Regex.Match(input, "(ab)|(cd)|(pq)|(xy)").Success

doesNotContainNotAllowed "asdf"
doesNotContainNotAllowed "absdf"
doesNotContainNotAllowed "ascdf"
doesNotContainNotAllowed "asdfpqz"
doesNotContainNotAllowed "asdfxyz"

let isNiceString input =
    countainsAtLeastThreeVowels input &&
    containsDuplicateCharacter input &&
    doesNotContainNotAllowed input

let input = File.ReadAllLines "input.txt"

input
|> Array.map isNiceString
|> Array.filter (id)
|> Array.length
