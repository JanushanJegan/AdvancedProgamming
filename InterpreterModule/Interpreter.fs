module InterpreterModule.Interpreter

open System
open Microsoft.FSharp.Core.Operators.Checked

type Rational = Rational of int * int  // Numerator and denominator
type Value = | Int of int | Float of float | Rational of int * int | Complex of float * float | Error of string
and terminal = Add | Sub | Mul | Div | Pow | Mod |  Lbr | Rbr | Eql | Num of Value | Var of string

exception LexerError of char
exception ParserError
exception VarUndefined of string

let mutable varMap = Map.empty<string, Value>
let getMap() = Map.empty<string, Value>
let strToList s = [for c in s -> c]
let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c
let isLetter c = Char.IsLetter c
let intVal c = int(Char.GetNumericValue c)

// Compute the greatest common divisor (GCD) for simplification
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
let setMode (mode: string) =
    match mode with
    | "Reset" -> 
        currentMode <- Reset
        varMap <- Map.empty  // Reset the variable map
    | "Rational" -> currentMode <- Rational
    | "Complex" -> currentMode <- Complex
    | _ -> failwith "Invalid mode"
    varMap  // Return the updated map


let toFloat = function
    | Int v -> float v
    | Float v -> v
    | Value.Complex (re, im) -> re  // Convert only the real part to float
    | _ -> 0.0  // Default case for other types

// Updated add, sub, mul, div functions
let add a b =
    match currentMode with
    | Rational -> addRational (toRational a) (toRational b)
    | Complex -> match a, b with
                 | Value.Complex (a1, b1), Value.Complex (a2, b2) -> Value.Complex (addComplex (a1, b1) (a2, b2))
                 | Value.Int a, Value.Complex (a2, b2) -> Value.Complex (addComplex (float a, 0.0) (a2, b2))
                 | Value.Complex (a1, b1), Value.Int b -> Value.Complex (addComplex (a1, b1) (float b, 0.0))
                 | _ -> Value.Error "Invalid operation for Complex mode"
    | _ -> match a, b with
           | Value.Int a, Value.Int b -> Value.Int(a + b)
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
    | _ -> match a, b with
           | Value.Int a, Value.Int b -> Value.Int(a - b)
           | _ -> Value.Float(toFloat a - toFloat b)

let mul a b =
    match currentMode with
    | Rational -> mulRational (toRational a) (toRational b)
    | Complex -> match a, b with
                 | Value.Complex (a1, b1), Value.Complex (a2, b2) -> Value.Complex (mulComplex (a1, b1) (a2, b2))
                 | Value.Int a, Value.Complex (a2, b2) -> Value.Complex (mulComplex (float a, 0.0) (a2, b2))
                 | Value.Complex (a1, b1), Value.Int b -> Value.Complex (mulComplex (a1, b1) (float b, 0.0))
                 | _ -> Value.Error "Invalid operation for Complex mode"
    | _ -> match a, b with
           | Value.Int a, Value.Int b -> Value.Int(a * b)
           | _ -> Value.Float(toFloat a * toFloat b)

let div a b =
    match currentMode with
    | Rational -> divRational (toRational a) (toRational b)
    | Complex -> match a, b with
                 | Value.Complex (a1, b1), Value.Complex (a2, b2) -> Value.Complex (divComplex (a1, b1) (a2, b2))
                 | Value.Int a, Value.Complex (a2, b2) -> Value.Complex (divComplex (float a, 0.0) (a2, b2))
                 | Value.Complex (a1, b1), Value.Int b -> Value.Complex (divComplex (a1, b1) (float b, 0.0))
                 | _ -> Value.Error "Invalid operation for Complex mode"
    | _ -> match a, b with
           | Value.Int a, Value.Int b when b <> 0 -> Value.Int(a / b)
           | _ -> Value.Float(toFloat a / toFloat b)



let modulus a b = match a, b with | Int a, Int b -> Int(a % b) | _ -> Float(toFloat a % toFloat b)

let pow a b =
    match a, b with
    | Int a, Int b -> Int(pown a b)
    | Value.Rational (num, den), Int b ->
        let numPow = pown num b
        let denPow = pown den b
        simplify (Value.Rational (numPow, denPow))
    | _ -> Float(toFloat a ** toFloat b)

// Function to set the
let rec scanInt(input, n) = 
    match input with
    | c::tail when isDigit c -> scanInt(tail, 10*n+(intVal c))
    | _ -> (input, n)
and scanFloat(input, decimal, pos) =
    match input with
    | c::tail when isDigit c -> scanFloat(tail, decimal+float(intVal c)/10.**pos, pos+1.)
    | _ -> (input, decimal)

and scanStr(input, varName) =
    match input with
    | c::tail when isLetter c || isDigit c -> scanStr(tail, varName + string c)
    | _ -> (input, varName)

let lexer input = 
    let rec scan input lastToken =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail Add
        | '-'::tail -> Sub :: scan tail Sub
        | '*'::'*'::tail | '^'::tail -> Pow :: scan tail Pow
        | '*'::tail -> Mul :: scan tail Mul
        | '/'::tail -> Div :: scan tail Div
        | '%'::tail -> Mod :: scan tail Mod
        | '('::tail -> 
            let tokens = if lastToken = Rbr || (match lastToken with Num _ | Var _ -> true | _ -> false) then [Lbr] else [Lbr]
            tokens @ scan tail Lbr
        | ')'::tail -> Rbr :: scan tail Rbr
        | '='::tail -> Eql :: scan tail Eql
        | c :: tail when isBlank c -> scan tail lastToken
        | c :: tail when isLetter c -> 
            let remInput, varName = scanStr(tail, string c)
            Var varName :: scan remInput (Var varName)
        | c :: tail when isDigit c -> 
            let remInput, n = scanInt(tail, intVal c)
            match remInput with
            | '/'::tail2 -> 
                let remInput2, d = scanInt(tail2, 0)
                Num(Value.Rational(n, d)) :: scan remInput2 (Num(Value.Rational(n, d)))
            | 'i'::rest -> Num(Value.Complex(float n, 1.0)) :: scan rest (Num(Value.Complex(float n, 1.0)))
            | '+'::tail2 | '-'::tail2 when List.exists (fun x -> x = 'i') tail2 ->
                let remInput2, m = scanInt(tail2, 0)
                Num(Value.Complex(float n, float m)) :: scan remInput2 (Num(Value.Complex(float n, float m)))
            | _ -> Num(Int n) :: scan remInput (Num(Int n))
        | _ -> raise (LexerError (List.head input))
    scan (strToList input) (Var "")  // Start with Var "" as the initial lastToken
    
// <S>        ::= <Var> "=" <E> | <E>
// <E>        ::= <T> <EOpt>
// <EOpt>     ::= "+" <T> <EOpt> | "-" <T> <EOpt> | <empty>
// <T>        ::= <F> <TOpt>
// <TOpt>     ::= "*" <F> <TOpt> | "/" <F> <TOpt> | <empty>
// <F>        ::== <NR> <FOpt>
// <FOpt>     ::= "^" <F> <FOpt> | <empty>
// <NR>       ::= <Num> | "-"<Num> | "-"<Var> | <Var> | "(" <E> "
let parser tList =
    let rec S tList =
        printfn "S: %A" tList
        match tList with
        | Var _ :: Eql :: tail -> 
            printfn "S: Handling variable assignment"
            E tail  // Handle variable assignment
        | _ -> 
            printfn "S: Parsing as an expression"
            E tList  // Otherwise, parse as an expression
    and E tList = 
        printfn "E: %A" tList
        let tList, value = T tList
        EOpt (tList, value)
    and EOpt (tList, value) = 
        printfn "EOpt: %A, value: %A" tList value
        match tList with
        | Add :: tail -> 
            printfn "EOpt: Parsing Add"
            let tList, tVal = T tail
            EOpt (tList, add value tVal)
        | Sub :: tail -> 
            printfn "EOpt: Parsing Sub"
            let tList, tVal = T tail
            EOpt (tList, sub value tVal)
        | _ -> 
            printfn "EOpt: Returning value"
            (tList, value)  // If no `+` or `-`, return the current value
    and T tList = 
        printfn "T: %A" tList
        let tList, value = F tList
        TOpt (tList, value)
    and TOpt (tList, value) =
        printfn "TOpt: %A, value: %A" tList value
        match tList with
        | Mul :: tail -> 
            printfn "TOpt: Parsing Mul"
            let tList, tVal = F tail
            TOpt (tList, mul value tVal)
        | Div :: tail -> 
            printfn "TOpt: Parsing Div"
            let tList, tVal = F tail
            TOpt (tList, div value tVal)
        | _ -> 
            printfn "TOpt: Returning value"
            (tList, value)  // If no `*` or `/`, return the current value
    and F tList = 
        printfn "F: %A" tList
        let tList, value = NR tList
        FOpt (tList, value)
    and FOpt (tList, value) =
        printfn "FOpt: %A, value: %A" tList value
        match tList with
        | Pow :: tail -> 
            printfn "FOpt: Parsing Pow"
            let tList, tVal = F tail
            FOpt (tList, pow value tVal)
        | _ -> 
            printfn "FOpt: Returning value"
            (tList, value)  // If no `^`, return the current value
    and NR tList =
        printfn "NR: %A" tList
        match tList with 
        | Lbr :: tail -> 
            printfn "NR: Parsing expression inside parentheses"
            // Parse expression inside parentheses
            let tList, value = E tail
            match tList with 
            | Rbr :: tail -> 
                printfn "NR: Successfully matched closing parenthesis"
                (tail, value)  // Successfully matched closing parenthesis
            | _ -> 
                printfn "NR: Missing closing parenthesis"
                raise ParserError  // Raise error if closing parenthesis is missing
        | Num(Value.Complex(re, im)) :: Var "i" :: tail -> 
            printfn "NR: Parsing complex number %A + %Ai" re im
            (tail, Value.Complex(re, im))
        | Sub :: Num(Value.Complex(re, im)) :: Var "i" :: tail -> 
            printfn "NR: Parsing negative complex number %A + %Ai" (-re) (-im)
            (tail, Value.Complex(-re, -im))
        | Num value :: tail -> 
            printfn "NR: Parsing number %A" value
            (tail, value)
        | Sub :: Num value :: tail -> 
            printfn "NR: Parsing negative number %A" value
            (tail, sub (Int 0) value)
        | Var varName :: tail -> 
            printfn "NR: Parsing variable %s" varName
            if Map.containsKey varName varMap then
                (tail, Map.find varName varMap)
            else
                raise (VarUndefined varName)
        | Sub :: Var varName :: tail -> 
            printfn "NR: Parsing negative variable %s" varName
            if Map.containsKey varName varMap then
                (tail, sub (Int 0) (Map.find varName varMap))
            else
                raise (VarUndefined varName)
        | _ -> 
            printfn "NR: Unexpected token sequence"
            raise ParserError  // Raise error for invalid token sequence
    S tList


let parseAndEval tList =
    let rec S tList = 
        match tList with
        | Var varName :: Eql :: tail -> 
            let tList, value = E tail
            varMap <- Map.add varName value varMap
            (tList, value)
        | _ -> E tList
    and E tList = 
        let tList, value = T tList
        EOpt (tList, value)
    and EOpt (tList, value) = 
        match tList with
        | Add :: tail -> 
            let tList, tVal = T tail
            EOpt (tList, add value tVal)
        | Sub :: tail -> 
            let tList, tVal = T tail
            EOpt (tList, sub value tVal)
        | _ -> (tList, value)
    and T tList = 
        let tList, value = F tList
        TOpt (tList, value)
    and TOpt (tList, value) =
        match tList with
        | Mul :: tail -> 
            let tList, tVal = F tail
            TOpt (tList, mul value tVal)
        | Div :: tail -> 
            let tList, tVal = F tail
            TOpt (tList, div value tVal)
        | _ -> (tList, value)
    and F tList = 
        let tList, value = NR tList
        FOpt (tList, value)
    and FOpt (tList, value) =
        match tList with
        | Pow :: tail -> 
            let tList, tVal = F tail
            FOpt (tList, pow value tVal)
        | _ -> (tList, value)
    and NR tList =
        match tList with 
        | Num(Value.Complex(re, im)) :: tail -> (tail, Value.Complex(re, im))
        | Sub :: Num(Value.Complex(re, im)) :: tail -> (tail, Value.Complex(-re, -im))
        | Num value :: tail -> (tail, value)
        | Sub :: Num value :: tail -> (tail, sub (Int 0) value)
        | Var varName :: tail -> (tail, Map.find varName varMap)
        | Sub :: Var varName :: tail -> (tail, sub (Int 0) (Map.find varName varMap))
        | Lbr :: tail -> 
            // Parse an expression within parentheses
            let tList, value = E tail
            match tList with 
            | Rbr :: tail -> (tail, value)
            | _ -> raise ParserError  // Raise error if closing parenthesis is missing
        | _ -> raise ParserError
    S tList


    
let rec printTokenList (lst:list<terminal>) : list<string> = 
    match lst with
    | head::tail -> printf $"{head.ToString()} "; printTokenList tail
    | [] -> printfn "EOL"; []
            
let main (input: string, vM) =
    varMap <- vM
    try
        let tokenList = lexer input
        printTokenList tokenList |> ignore
        let _, result = parser tokenList
        ($"Result = {result}", varMap)
    with
        | LexerError(c) -> $"Lexer Error, invalid token {c}", varMap
        | ParserError -> "Error parsing", varMap
        | VarUndefined(v) -> $"Variable {v} is not defined", varMap
        | :? OverflowException -> "Overflow error, exceeded max value for int32", varMap


// Test function to verify complex number operations
let testComplexOperations2 () =
    // Set mode to Complex
    setMode "Complex" |> ignore

    // Test addition of complex numbers
    let result1 = main("3+4i + 1+2i", varMap)
    printfn "%A" result1  // Expected: Result = Complex(4.0, 6.0)

    let result2 = main("5+6i - 2+3i", varMap)
    printfn "%A" result2  // Expected: Result = Complex(3.0, 3.0)

    let result3 = main("1+2i * 3+4i", varMap)
    printfn "%A" result3  // Expected: Result = Complex(-5.0, 10.0)

    let result4 = main("1+2i / 3+4i", varMap)
    printfn "%A" result4  // Expected: Result = Complex(0.44, 0.08)
    



    (*
    // Test subtraction of complex numbers
    let result2 = main("5+6i - 2+3i", varMap)
    printfn "%A" result2  // Expected: Result = Complex(3.0, 3.0)

    // Test multiplication of complex numbers
    let result3 = main("1+2i * 3+4i", varMap)
    printfn "%A" result3  // Expected: Result = Complex(-5.0, 10.0)

    // Test division of complex numbers
    let result4 = main("1+2i / 3+4i", varMap)
    printfn "%A" result4  // Expected: Result = Complex(0.44, 0.08)
    *)

let testComplexExpression () =
    // Set mode to Complex
    setMode "Complex" |> ignore

    // Evaluate the complex expression
    let result = main("((3+4i) * (5-2i) + (7+3i)) / ((1+2i) * (3-i))", varMap)
    printfn "%A" result  // Expected: Result = Complex(4.7, -1.3)

// Call the test function

let testRationalOperations () =
    // Set mode to Rational
    setMode "Rational" |> ignore

    // Test addition of rational numbers
    let result1 = main("1/2 + 1/3", varMap)
    printfn "%A" result1  // Expected: Result = Rational(5, 6)

    // Test subtraction of rational numbers
    let result2 = main("3/4 - 1/4", varMap)
    printfn "%A" result2  // Expected: Result = Rational(1, 2)

    // Test multiplication of rational numbers
    let result3 = main("2/3 * 3/4", varMap)
    printfn "%A" result3  // Expected: Result = Rational(1, 2)

    // Test division of rational numbers
    let result4 = main("4/5 / 2/3", varMap)
    printfn "%A" result4  // Expected: Result = Rational(6, 5)



testComplexExpression()

let testComplexRationalOperations2 () =
    // Set mode to Rational
    setMode "Rational" |> ignore

    // Test addition and multiplication with brackets
    let result1 = main("(1/2 + 1/3) * 2/5", varMap)
    printfn "%A" result1  // Expected: Result = 1/3

    
    // Test nested brackets
    let result2 = main("((1/3 + 1/4) - 1/2) + 1/6", varMap)
    printfn "%A" result2  // Expected: Result = 1/4


    // Test power of a rational number
    let result3 = main("(2/3)**2 + 1/9", varMap)
    printfn "%A" result3  // Expected: Result = 5/9
    

    let result4 = main("(4/5 + 1/2) / (3/4 - 2/3)", varMap)
    printfn "%A" result4  // Expected: Result = 78/5

    // Test multiple nested powers
    let result5 = main("((2/3)**3 + 1/27) * (3/2)**2", varMap)
    printfn "%A" result5  // Expected: Result = 3/4
    

    
    (*
    // Test addition and multiplication with brackets
    let result1 = main("(1/2 + 1/3) * 2/5", varMap)
    printfn "%A" result1  // Expected: Result = Rational(5, 15) * 2/5 = Rational(2, 15)

    
    // Test nested brackets
    let result2 = main("((1/3 + 1/4) - 1/2) + 1/6", varMap)
    printfn "%A" result2  // Expected: Result = ((4/12 + 3/12) - 6/12) + 2/12 = Rational(3, 12) = Rational(1, 4)


    // Test power of a rational number
    let result3 = main("(2/3)**2 + 1/9", varMap)
    printfn "%A" result3  // Expected: Result = (4/9) + (1/9) = Rational(5, 9)
    *)
    
    
 
    (*
    // Test a more complex expression with division, addition, and brackets
    let result4 = main("(4/5 + 1/2) / (3/4 - 2/3)", varMap)
    printfn "%A" result4  // Expected: Result = (8/10 + 5/10) / (9/12 - 8/12) = Rational(13, 10) / Rational(1, 12) = Rational(156, 10) = Rational(39, 5)

    // Test multiple nested powers
    let result5 = main("((2/3)**3 + 1/27) * (3/2)**2", varMap)
    printfn "%A" result5  // Expected: Result = ((8/27 + 1/27) * 9/4) = (9/27 * 9/4) = Rational(27, 36) = Rational(3, 4)
    *)

// Call the test function
let testCases = [
    "(1/2 + 1/3) * 2/5";
    "((1/3 + 1/4) - 1/2) + 1/6";
    "(2/3) ^ 2 + 1/9";
    "(4/5 + 1/2) / (3/4 - 2/3)";
    "((2/3) ^ 3 + 1/27) * ((3/2) ^ 2)"
](*let testLexer input =
    try
        let tokens = lexer (strToList input)
        printfn "Input: %s" input
        printfn "Tokens: %A" tokens
    with
    | LexerError c -> printfn "LexerError: Unexpected character '%c'" c

testCases |> List.iter testLexer
*)