open System.IO
open System.Text.RegularExpressions

type Gate =
    | Constant of uint16
    | AndGate of Gate * Gate
    | OrGate of Gate * Gate
    | LeftShift of Gate * int
    | RightShift of Gate * int
    | NotGate of Gate

let rec evalGate gate =
    match gate with
    | Constant i -> i
    | AndGate(a, b) -> (evalGate a) &&& (evalGate b)
    | OrGate(a, b) -> (evalGate a) ||| (evalGate b)
    | LeftShift(a, b) -> (evalGate a) <<< b
    | RightShift(a, b) -> (evalGate a) >>> b
    | NotGate(a) -> ~~~(evalGate a)

let x = Constant 123us
let y = Constant 456us
let d = AndGate (x, y)
let e = OrGate (x, y)
let f = LeftShift (x, 2)
let g = RightShift (y, 2)
let h = NotGate x
let i = NotGate y

evalGate x
evalGate y
evalGate d
evalGate e
evalGate f
evalGate g
evalGate h
evalGate i

let matcher = Regex "(^\d+ -> \w+$)|(^\w+ (AND|OR) \w+ -> \w+$)|(^\w+ (L|R)SHIFT \d+ -> \w+$)|(^NOT \w+ -> \w+$)"