module InterpreterModule.Interpreter

open System
open Microsoft.FSharp.Core.Operators.Checked
open InterpreterModule.Types

let mutable varMap = Map.empty<string, Value>
let mutable funcMap = Map.empty<string, string * terminal list>


let strToList s = [for c in s -> c]
let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c
let isLetter c = Char.IsLetter c
let intVal c = int(Char.GetNumericValue c)

let rec scanInt(input, n) =
    match input with
    | '-'::c::tail when isDigit c && n=0 -> scanInt(tail, 10*n-(intVal c))
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

//check if list of chars starts with keyword
let startsWith (keyword: string) (input: char list) =
    let keywordChars = strToList keyword
    let rec check kw input =
        match kw, input with
        | [], _ -> true
        | kc::kTail, ic::iTail when kc = ic -> check kTail iTail
        | _ -> false
    check keywordChars input

//drop first n chars from list
let drop n list =
    list |> List.skip n

let lexNumber c tail =
    let remInput, n = scanInt(tail, intVal c)
    match remInput with
    | '.'::tail -> let remInput, decimal = scanFloat(tail, 0., 1.)
                   Float(decimal+float n), remInput
    | _ -> Int n, remInput



let lexer input =
    let rec scan input lastToken =
        match input with
        | [] -> []  // End of input
        | '-'::c :: tail when isDigit c -> let n, remInput = lexNumber c tail // keep - attached to numbers, helps with algebraic manipulation later on
                                           let n = Num (sub (Int 0) n)
                                           match remInput with | a::_ when (isLetter a && a<>'E') -> Add :: n :: Mul :: scan remInput Mul | _ -> Add :: n :: scan remInput n // allows implicit multiplication of variables
        | '+' :: tail -> Add :: scan tail Add
        | '-' :: tail -> Sub :: scan tail Sub
        | '*' :: '*' :: tail | '^' :: tail -> Pow :: scan tail Pow
        | '*' :: tail -> Mul :: scan tail Mul
        | '/' :: tail -> Div :: scan tail Div
        | '%' :: tail -> Mod :: scan tail Mod
        | '(' :: tail ->
            // Handle single-bracket complex numbers
            printfn "Doing ("
            let remInput, realPart = scanInt(tail, 0)
            printfn $"remInput: %A{remInput}, imagPart: %A{realPart}"
            match remInput with
            | '-' :: imagHead :: remTail when isDigit imagHead && currentMode=Mode.Complex ->
                printfn "Doing -"
                let remInput2, imagPart = scanInt(imagHead :: remTail, 0)
                printfn $"remInput: %A{remInput2}, imagPart: %A{imagPart}"
                if List.head remInput2 = 'i' && List.head (List.tail remInput2) = ')' then
                    Num(Value.Complex(float realPart, -float imagPart)) :: scan (List.tail (List.tail remInput2)) lastToken
                else raise (LexerError (List.head remInput2))
            | '+' :: imagHead :: remTail when isDigit imagHead && currentMode=Mode.Complex ->
                let remInput2, imagPart = scanInt(imagHead :: remTail, 0)
                if List.head remInput2 = 'i' && List.head (List.tail remInput2) = ')' then
                    Num(Value.Complex(float realPart, float imagPart)) :: scan (List.tail (List.tail remInput2)) lastToken
                else raise (LexerError (List.head remInput2))
            | _ ->
                // Not a complex number, assume it is a normal expression
                Lbr :: scan tail Lbr
        | ')' :: tail -> Rbr :: scan tail Rbr
        | '=' :: tail -> Eql :: scan tail Eql
        | ',' :: tail -> Comma :: scan tail Comma
        | _ when startsWith "cos" input -> Cos :: scan (drop 3 input) Cos
        | _ when startsWith "sin" input -> Sin :: scan (drop 3 input) Sin
        | _ when startsWith "tan" input -> Tan :: scan (drop 3 input) Tan
        | _ when startsWith "exp" input -> Exp :: scan (drop 3 input) Exp
        | _ when startsWith "log" input -> Log :: scan (drop 3 input) Log
        | _ when startsWith "integral" input -> Integral :: scan (drop 8 input) Integral
        | c :: tail when isBlank c -> scan tail lastToken  // Skip whitespace
        | c :: tail when isLetter c ->
            let remInput, varName = scanStr(tail, string c)
            Var varName :: scan remInput (Var varName)
        // | c :: tail when isDigit c ->
        //     let remInput, n = scanInt(tail, intVal c)
        //     match remInput with
        //     | '-' :: imagHead :: remTail when isDigit imagHead ->
        //         // Handle complex numbers with negative imaginary parts
        //         let remInput2, imag = scanInt(imagHead :: remTail, 0)
        //         if List.head remInput2 = 'i' then
        //             Num(Value.Complex(float n, -float imag)) :: scan (List.tail remInput2) lastToken
        //         else
        //             raise (LexerError (List.head remInput2))
        //     | '+' :: imagHead :: remTail when isDigit imagHead ->
        //         // Handle complex numbers with positive imaginary parts
        //         let remInput2, imag = scanInt(imagHead :: remTail, 0)
        //         if List.head remInput2 = 'i' then
        //             Num(Value.Complex(float n, float imag)) :: scan (List.tail remInput2) lastToken
        //         else
        //             raise (LexerError (List.head remInput2))
        //     | 'i' :: rest -> Num(Value.Complex(float n, 1.0)) :: scan rest lastToken
        //     | _ -> Num(Int n) :: scan remInput (Num(Int n))  // Default to integer
        // |  -> raise (LexerError (List.head input))  // Invalid token
        | c :: tail when isDigit c -> let n, remInput = lexNumber c tail
                                      match remInput with | a::_ when (isLetter a && a<>'E') -> Num n :: Mul :: scan remInput Mul | _ -> Num n :: scan remInput (Num n) // allows implicit multiplication of variables
        | _ -> printfn $"Remaining tokens: %A{input}"; raise (LexerError(List.head input))
    scan (strToList input) (Var "")  // Start with Var "" as the initial lastToken

// Grammar in BNF:
// <S>        ::= <Var> "=" <E> | <Var> "(" <Var> ")" "=" <E> | <E>
// <E>        ::= <T> <EOpt>
// <EOpt>     ::= "+" <T> <EOpt> | "-" <T> <EOpt> | "%" <T> <EOpt> | <empty>
// <T>        ::= <F> <TOpt>
// <TOpt>     ::= "*" <F> <TOpt> | "/" <F> <TOpt> | <empty>
// <F>        ::== <NR> <FOpt>
// <FOpt>     ::= "^"|"**" <F> <FOpt> | <empty>
// <NR>       ::= "+" <NR> | "-" <NR> | <Num> | <Var> | <Var> "(" <E> ")" | <Cos|Sin|Tan|Exp|Log> "(" <E> ")" | "(" <E> ")"


let parser tList =
    let rec S tList =
        match tList with
        | Var _ :: Eql :: tail -> E tail
        | Var _ :: Lbr :: Var v :: Rbr :: Eql :: tail ->
            let origMap = varMap
            varMap <- Map.add v (Int 0) varMap //add Var v to map temporary so function expression can parse
            let tList = E tail
            varMap <- origMap
            tList
        | Integral :: Lbr :: Var f :: Comma :: Num a :: Comma :: Num b :: Comma :: Num steps :: Rbr :: tail ->
            if not (funcMap.ContainsKey(f)) then
                raise (VarUndefined(f))
            else
                tail //integral parsed
        | _ -> E tList
    and E tList = (T >> EOpt) tList
    and EOpt tList =
        match tList with
        | (Add|Sub|Mod) :: tail -> (T >> EOpt) tail
        | _ -> tList
    and T tList = (F >> TOpt) tList
    and TOpt tList =
        match tList with
        | (Mul|Div) :: tail -> (F >> TOpt) tail
        | _ -> tList
    and F tList = (NR >> FOpt) tList
    and FOpt tList =
        match tList with
        | (Pow|Ent) :: tail -> (F >> FOpt) tail
        | _ -> tList
    and NR tList =
        match tList with
        | (Add|Sub) :: tail -> NR tail // handles repeated signs like 2+-3 or unary signs like -cos
        | Num _ :: tail -> tail
        | (Cos|Sin|Tan|Exp|Log) :: Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
        | Var v :: Lbr :: tail -> match E tail with | Rbr :: tail -> (if funcMap.ContainsKey(v) then tail else raise (VarUndefined(v))) | _ -> raise ParserError // function f(x)
        | Var v :: tail -> if varMap.ContainsKey(v) then tail else raise (VarUndefined(v)) // variable x
        | Lbr :: tail -> let x = S tail
                         match x with | Rbr :: tail -> tail | _ -> raise ParserError // bracketed expression
        | _ -> raise ParserError
    S tList


let parseAndEval tList =
    let rec S tList =
        match tList with
        | Var varName :: Eql :: tail ->
            let tList, value = E tail
            varMap <- Map.add varName value varMap
            (tList, value)
        | Var funcName :: Lbr :: Var n :: Rbr :: Eql :: tail ->
            funcMap <- Map.add funcName (n,tail) funcMap
            (tList, Int 0) // got to return something
        | _ -> E tList
    and E tList = (T >> EOpt) tList
    and EOpt (tList, value) =
        match tList with
        | Add :: tail -> let tLst, tVal = T tail
                         EOpt (tLst, add value tVal)
        | Sub :: tail -> let tLst, tVal = T tail
                         EOpt (tLst, sub value tVal)
        | Mod :: tail -> let tLst, tVal = T tail
                         EOpt (tLst, modulus value tVal)
        | _ -> (tList, value)
    and T tList = (F >> TOpt) tList
    and TOpt (tList, value) =
        match tList with
        | Mul :: tail -> let tLst, tVal = F tail
                         TOpt (tLst, mul value tVal)
        | Div :: tail -> let tLst, tVal = F tail
                         TOpt (tLst, div value tVal)
        | _ -> (tList, value)
    and F tList = (NR >> FOpt) tList
    and FOpt (tList, value) =
        match tList with
        | Pow :: tail -> let tLst, tVal = F tail
                         FOpt (tLst, pow value tVal)
        | Ent :: tail -> let tLst, tVal = F tail  // scientific notation, E equivalent to *10**
                         FOpt (tLst, mul value (pow (Float 10.) tVal))
        | _ -> (tList, value)
    and NR tList =
        match tList with
        | Sub :: tail -> let tail, value = NR tail
                         tail, sub (Int 0) value
        | Add :: tail -> NR tail

        | Num value :: tail -> (tail, value)
        | Cos|Sin|Tan|Exp|Log as f :: Lbr :: tail ->
            let tList', num = E tail
            match tList' with
            | Rbr :: tail -> match f with
                             | Cos -> (tail, Float (Math.Cos(toFloat num)))
                             | Sin -> (tail, Float (Math.Sin(toFloat num)))
                             | Tan -> (tail, Float (Math.Tan(toFloat num)))
                             | Log -> (tail, Float (Math.Log(toFloat num)))
                             | Exp -> (tail, Float (Math.Exp(toFloat num)))

        | Var funcName :: Lbr :: tail -> let tLst, n = E tail
                                         match tLst with
                                         | Rbr :: tail ->
                                             let argName, func = Map.find funcName funcMap
                                             let origMap = varMap  // store old varMap so vars with name of function variable aren't overwritten while evaluating func
                                             varMap <- Map.add argName n varMap
                                             let _,value = E func
                                             varMap <- origMap
                                             (tail, value)
                                         | _ -> raise ParserError
        | Var varName :: tail -> (tail, Map.find varName varMap)
        | Lbr :: tail -> let tLst, value = E tail
                         match tLst with
                          | Rbr :: tail -> (tail, value)
                          | _ -> raise ParserError
        | Integral :: Lbr :: Var f :: Comma :: Num a :: Comma :: Num b :: Comma :: Num steps :: Rbr :: tail ->
            printfn "Integral Input: %s, Range: (%A, %A), Steps: %A" f a b steps
            let stepCount =
                match steps with
                | Int s when s > 0 -> s
                | Float s when s > 0.0 -> int s
                | _ -> raise (ArgumentException "Number of steps must be a positive integer.")
            if not (funcMap.ContainsKey(f)) then
                raise (VarUndefined(f))
            else
                let h = (toFloat b - toFloat a) / float stepCount
                if h = 0.0 then
                    (tail, Float 0.0) //range 0, integral 0
                else
                    //Inline func eval for f(x) -- MOVE OUTSIDE?
                    let argName, funcTokens = funcMap.[f]
                    let evaluateFunction x =
                        let origMap = varMap
                        varMap <- Map.add argName (Float x) varMap
                        let rec evalTokens tokens =
                            match tokens with
                            | Var v :: Pow :: Num exp :: rest when varMap.ContainsKey(v) ->
                                //`Var v ^ Num exp`
                                let baseValue = varMap.[v]
                                let expValue = toFloat exp
                                (pow baseValue (Float expValue), rest)
                            | Var v :: Pow :: rest when varMap.ContainsKey(v) ->
                                //`Var v ^ <expression>`
                                let baseValue = varMap.[v]
                                let expValue, remainingTokens = evalTokens rest
                                (pow baseValue expValue, remainingTokens)
                            | Num baseValue :: Pow :: Num exp :: rest ->
                                //`Num baseValue ^ Num exp`
                                let baseValueFloat = toFloat baseValue
                                let expValue = toFloat exp
                                (Float (baseValueFloat ** expValue), rest)
                            | Num baseValue :: Pow :: rest ->
                                //Handle `Num baseValue ^ <expression>`
                                let baseValueFloat = toFloat baseValue
                                let expValue, remainingTokens = evalTokens rest
                                (Float (baseValueFloat ** toFloat expValue), remainingTokens)
                            | Cos :: Lbr :: rest ->
                                //`cos(expression)`
                                let value, remainingTokens = evalTokens rest
                                match remainingTokens with
                                | Rbr :: tail -> (Float (System.Math.Cos(toFloat value)), tail)
                                | _ -> raise ParserError
                            | Sin :: Lbr :: rest ->
                                //`sin(expression)`
                                let value, remainingTokens = evalTokens rest
                                match remainingTokens with
                                | Rbr :: tail -> (Float (System.Math.Sin(toFloat value)), tail)
                                | _ -> raise ParserError
                            | Exp :: Lbr :: rest ->
                                //`exp(expression)`
                                let value, remainingTokens = evalTokens rest
                                match remainingTokens with
                                | Rbr :: tail -> (Float (System.Math.Exp(toFloat value)), tail)
                                | _ -> raise ParserError
                            | Num value :: rest ->
                                //generic numberfs
                                (value, rest)
                            | Var v :: rest when varMap.ContainsKey(v) ->
                                //generic vars
                                (varMap.[v], rest)
                            | Add :: rest ->
                                let left, remLeft = evalTokens rest
                                let right, remRight = evalTokens remLeft
                                (add left right, remRight)
                            | Sub :: rest ->
                                let left, remLeft = evalTokens rest
                                let right, remRight = evalTokens remLeft
                                (sub left right, remRight)
                            | Mul :: rest ->
                                let left, remLeft = evalTokens rest
                                let right, remRight = evalTokens remLeft
                                (mul left right, remRight)
                            | Div :: rest ->
                                let left, remLeft = evalTokens rest
                                let right, remRight = evalTokens remLeft
                                (div left right, remRight)
                            | _ -> raise ParserError
                        let result, _ = evalTokens funcTokens
                        varMap <- origMap
                        //debug
                        printfn "x = %f, f(x) = %f" x (toFloat result)
                        toFloat result
                    //trapezoid integration
                    let trapezoidSum =
                        Seq.init (stepCount + 1) (fun i -> toFloat a + float i * h) //x vals
                        |> Seq.map evaluateFunction
                        |> Seq.pairwise
                        |> Seq.mapi (fun i (y1, y2) ->
                            let area = 0.5 * h * (y1 + y2)
                            printfn "Trapezoid %d: y1 = %f, y2 = %f, Area = %f" i y1 y2 area //Debug steps
                            area)
                        |> Seq.sum
                    (tail, Float trapezoidSum)
        | _ -> raise ParserError
    snd (S tList)

let rec printTokenList (lst:list<terminal>) : list<string> =
    match lst with
    | head::tail -> printf $"{head.ToString()} "; printTokenList tail
    | [] -> printfn "EOL"; []
let main (input:string, vM, fM, mode)  =
    varMap <- vM; funcMap <- fM
    currentMode <- mode
    try
        let tokenList = lexer input
        printTokenList tokenList |> ignore
        parser tokenList |> ignore
        let result = parseAndEval tokenList
        $"Result = {result}", varMap, funcMap
    with
        | LexerError(c) -> $"Lexer Error, invalid token {c}", varMap, funcMap
        | ParserError -> "Error parsing", varMap, funcMap
        | VarUndefined(v) -> $"Variable {v} is not defined", varMap, funcMap
        | :? OverflowException -> "Overflow error, exceeded max value for int32", varMap, funcMap

let plot (input:string, minX:string, maxX:string, vM, fM)  =
    try
        varMap <- vM; funcMap <- fM
        let tokenList = lexer input
        parseAndEval tokenList |> ignore
        let minX, maxX = toFloat (lexer minX |> parseAndEval), toFloat (lexer maxX |> parseAndEval)  // parses minX and maxX as numbers
        let xVals = [for i in 0 .. 999 -> minX + (float i * (maxX-minX)/999.)] // creates 1000 x values to plot over the x range
        match tokenList with
            | Var fn :: Lbr :: Var _ :: Rbr :: Eql :: _ ->  // use parser to evaluate function at points by calling e.g. y(2)
                "", [for x in xVals -> (float x, toFloat(parseAndEval([Var fn; Lbr; Num(Float(x)); Rbr])))]
            | Integral :: Lbr :: Var f :: Comma :: Num a :: Comma :: Num b :: Comma :: Num steps :: Rbr :: _ ->
                let stepCount = int (toFloat steps)
                let h = (toFloat b - toFloat a) / float stepCount
                let points =
                    [for i in 0 .. stepCount ->
                        let x = toFloat a + float i * h
                        (x, toFloat(parseAndEval([Var f; Lbr; Num(Float(x)); Rbr])))]
                "", points
            | _ -> "", []
    with
        | LexerError(c) -> $"Lexer Error, invalid token {c}", []
        | ParserError -> "Error parsing", []
        | VarUndefined(v) -> $"Variable {v} is not defined", []
        | :? OverflowException -> "Overflow error, exceeded max value for int32", []


let differentiate(input:string, vM, fM) =
    varMap <- vM; funcMap <- fM
    try
        let tokenList = lexer input
        parser tokenList |> ignore
        match tokenList with
        | Var y :: Lbr :: Var x :: Rbr :: Eql :: tail ->
            $"d{y}/d{x} = {Differentiator.diffToString tail x}", varMap, funcMap
        | _ -> "Invalid Equation", varMap, funcMap

    with
        | LexerError(c) -> $"Lexer Error, invalid token {c}", varMap, funcMap
        | ParserError -> "Error parsing", varMap, funcMap
        | VarUndefined(v) -> $"Variable {v} is not defined", varMap, funcMap
        | :? OverflowException -> "Overflow error, exceeded max value for int32", varMap, funcMap

let getTangentAtPoint (input: string, xVal: string, vM, fM) =
    try
        varMap <- vM; funcMap <- fM
        let tokenList = lexer input
        parseAndEval tokenList |> ignore
        let xVal = lexer xVal |> parseAndEval  // parse xVal as a number

        match tokenList with
        | Var y :: Lbr :: Var x :: Rbr :: Eql :: tail ->
                printfn $"Gradient of %A{tokenList} at {xVal}"
                let yVal = toFloat(parseAndEval([Var y; Lbr; Num xVal; Rbr])) // gets y value of function at tangent x
                let differentiated = Differentiator.diffAndSimplify tail x  // differentiates equation
                parseAndEval (Var y :: Lbr :: Var x :: Rbr :: Eql :: differentiated) |> ignore
                let m = toFloat (parseAndEval [Var y; Lbr; Num xVal; Rbr])
                printfn $"Differentiated to %A{differentiated}, Gradient at {xVal} = {m}"
                let tangentEqn = $"{y}({x}) = {m} * ({x} - {toFloat xVal}) + {yVal}"
                let tangentEqnSimp = Differentiator.simplifyToString (lexer $"{m} * ({x} - {toFloat xVal}) + {yVal}")
                let roundCoeffs (input: string) = Text.RegularExpressions.Regex.Replace(input, @"\d+(\.\d+)?", fun m -> Math.Round ((float m.Value), 3) |> string)  // rounds numbers in equation
                printfn $"Tangent Equation: {tangentEqn} -> {tangentEqnSimp} -> {roundCoeffs tangentEqnSimp}"
                $"{y}({x}) = {roundCoeffs tangentEqnSimp}", ""
                // Tangent Equation: y(x) = 45426.093625176574 * (x - 7) + 32768 -> 45426.093625176574x - 285214.655376236 -> 45426.094x - 285214.655
        | _ -> "", "Invalid Equation"
    with
        | LexerError(c) -> "", $"Lexer Error, invalid token {c}"
        | ParserError -> "", "Error parsing"
        | VarUndefined(v) -> "", $"Variable {v} is not defined"
        | :? OverflowException -> "", "Overflow error, exceeded max value for int32"

