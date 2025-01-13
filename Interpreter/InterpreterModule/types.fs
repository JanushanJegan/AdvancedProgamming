module InterpreterModule.Types

type Value = | Int of int | Float of float| Rational of int * int | Complex of float * float | Error of string
and terminal = Add | Sub | Mul | Div | Pow | Mod | Lbr | Rbr | Eql | Cos | Sin | Tan | Exp | Ent | Log | Comma | Integral | Num of Value | Var of string

exception LexerError of char
exception ParserError
exception VarUndefined of string
exception InvalidEquation
let mutable varMap = Map.empty<string, Value>

let rec gcd a b = if b = 0 then abs a else gcd b (a % b)

let simplify = function
    | Rational (num, den) ->
        let divisor = gcd num den
        Rational (num / divisor, den / divisor)
    | value -> value  // Return the value as-is if it's not a Rational

let toRational = function
    | Int v -> Rational (v, 1)
    | Float v -> Rational (int v, 1)
    | Rational (num, den) -> Rational (num, den)
    | value -> value  // Return the value as-is if it's not convertible to Rational

let addRational a b =
    match a, b with
    | Rational (num1, den1), Rational (num2, den2) ->
        let num = num1 * den2 + num2 * den1
        let den = den1 * den2
        simplify (Rational (num, den))
    | _ -> Error "Invalid operation for non-rational values"

let subRational a b =
    match a, b with
    | Rational (num1, den1), Rational (num2, den2) ->
        let num = num1 * den2 - num2 * den1
        let den = den1 * den2
        simplify (Rational (num, den))
    | _ -> Error "Invalid operation for non-rational values"

let mulRational a b =
    match a, b with
    | Rational (num1, den1), Rational (num2, den2) ->
        simplify (Rational (num1 * num2, den1 * den2))
    | _ -> Error "Invalid operation for non-rational values"

let divRational a b =
    match a, b with
    | Rational (num1, den1), Rational (num2, den2) ->
        simplify (Rational (num1 * den2, den1 * num2))
    | _ -> Error "Invalid operation for non-rational values"
// Complex number operations
let addComplex (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
let subComplex (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)
let mulComplex (a1, b1) (a2, b2) = (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)
let divComplex (a1, b1) (a2, b2) =
    let denom = a2 * a2 + b2 * b2
    ((a1 * a2 + b1 * b2) / denom, (b1 * a2 - a1 * b2) / denom)

type Mode = Reset | Rational | Complex
let mutable currentMode = Reset  // Default mode is Reset

// Function to set the mode


let toFloat = function
    | Int v -> float v
    | Float v -> v
    | Value.Complex (re, im) -> re  // Convert only the real part to float
    | _ -> 0.0  // Default case for other types


let setMode (mode: string) =
    match mode with
    | "Reset" ->
        currentMode <- Reset
        varMap <- Map.empty  // Reset the variable map
    | "Rational" -> currentMode <- Rational
    | "Complex" -> currentMode <- Complex
    | _ -> failwith "Invalid mode"
    varMap  // Return the updated map





// Updated add, sub, mul, div functions
let add a b =
    match currentMode with
    | Rational -> addRational (toRational a) (toRational b)
    | Complex -> match a, b with
                 | Value.Complex (a1, b1), Value.Complex (a2, b2) -> Value.Complex (addComplex (a1, b1) (a2, b2))
                 | Value.Int a, Value.Complex (a2, b2) -> Value.Complex (addComplex (float a, 0.0) (a2, b2))
                 | Value.Complex (a1, b1), Value.Int b -> Value.Complex (addComplex (a1, b1) (float b, 0.0))
                 | _ -> Value.Error "Invalid operation for Complex mode"
    | _ ->
        match a, b with
        | Value.Int a, Value.Int b -> Value.Int(int (a + b))
        | Value.Float a, Value.Float b -> Value.Float(a + b)
        | Value.Int a, Value.Float b -> Value.Float(float a + b)
        | Value.Float a, Value.Int b -> Value.Float(a + float b)
        | _ -> Value.Error "Invalid operation"


let sub a b =
    match currentMode with
    | Rational -> subRational (toRational a) (toRational b)
    | Complex -> match a, b with
                 | Value.Complex (a1, b1), Value.Complex (a2, b2) -> Value.Complex (subComplex (a1, b1) (a2, b2))
                 | Value.Int a, Value.Complex (a2, b2) -> Value.Complex (subComplex (float a, 0.0) (a2, b2))
                 | Value.Complex (a1, b1), Value.Int b -> Value.Complex (subComplex (a1, b1) (float b, 0.0))
                 | _ -> Value.Error "Invalid operation for Complex mode"
    | _ ->
        match a, b with
        | Value.Int a, Value.Int b -> Value.Int(int (a - b))
        | _ -> Value.Float(toFloat a - toFloat b)


let mul a b =
    match currentMode with
    | Rational -> mulRational (toRational a) (toRational b)
    | Complex -> match a, b with
                 | Value.Complex (a1, b1), Value.Complex (a2, b2) -> Value.Complex (mulComplex (a1, b1) (a2, b2))
                 | Value.Int a, Value.Complex (a2, b2) -> Value.Complex (mulComplex (float a, 0.0) (a2, b2))
                 | Value.Complex (a1, b1), Value.Int b -> Value.Complex (mulComplex (a1, b1) (float b, 0.0))
                 | _ -> Value.Error "Invalid operation for Complex mode"
    | _ ->
        match a, b with
        | Value.Int a, Value.Int b -> Value.Int(int (a * b))
        | _ -> Value.Float(toFloat a * toFloat b)

let div a b =
    match currentMode with
    | Rational -> divRational (toRational a) (toRational b)
    | Complex -> match a, b with
                 | Value.Complex (a1, b1), Value.Complex (a2, b2) -> Value.Complex (divComplex (a1, b1) (a2, b2))
                 | Value.Int a, Value.Complex (a2, b2) -> Value.Complex (divComplex (float a, 0.0) (a2, b2))
                 | Value.Complex (a1, b1), Value.Int b -> Value.Complex (divComplex (a1, b1) (float b, 0.0))
                 | _ -> Value.Error "Invalid operation for Complex mode"
    | _ ->
        match a, b with
        | Value.Int a, Value.Int b when b <> 0 -> Value.Int(int a / int b)
        | _ -> Value.Float(toFloat a / toFloat b)



let modulus a b = match a, b with | Int a, Int b -> Int(a % b) | _ -> Float(toFloat a % toFloat b)


let pow a b =
    match a, b with
    | Int a, Int b -> Value.Int(int (pown a b))
    | Value.Rational (num, den), Int b ->
        let numPow = pown num b
        let denPow = pown den b
        simplify (Value.Rational (numPow, denPow))
    | _ -> Value.Float(toFloat a ** toFloat b)



