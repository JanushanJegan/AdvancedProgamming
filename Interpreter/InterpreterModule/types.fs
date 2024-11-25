module InterpreterModule.Types

type Value = | Int of int | Float of float
and terminal = Add | Sub | Mul | Div | Pow | Mod | Lbr | Rbr | Eql | Cos | Sin | Tan | Exp | Ent | Log | Comma | Integral | Num of Value | Var of string

exception LexerError of char
exception ParserError
exception VarUndefined of string
exception InvalidEquation

let toFloat = function | Int v -> float v | Float v -> v
let add a b = match a, b with | Int a, Int b -> Int(a + b) | _ -> Float(toFloat a + toFloat b)
let sub a b = match a, b with | Int a, Int b -> Int(a - b) | _ -> Float(toFloat a - toFloat b)
let mul a b = match a, b with | Int a, Int b -> Int(a * b) | _ -> Float(toFloat a * toFloat b)
let div a b = match a, b with | Int a, Int b when b <> 0 -> Int(a / b) | _ -> Float(toFloat a / toFloat b)
let pow a b = match a, b with | Int a, Int b -> Int(pown a b)  | _ -> Float(toFloat a ** toFloat b)
let modulus a b = match a, b with | Int a, Int b -> Int(a % b) | _ -> Float(toFloat a % toFloat b)
