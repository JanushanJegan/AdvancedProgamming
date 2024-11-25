module InterpreterModule.Interpreter

open System
open Microsoft.FSharp.Core.Operators.Checked

type Value = | Int of int | Float of float
and terminal = Add | Sub | Mul | Div | Pow | Mod | Lbr | Rbr | Eql | Cos | Sin | Tan | Exp | Log | Comma | Integral | Num of Value | Var of string

exception LexerError of char
exception ParserError
exception VarUndefined of string

let mutable varMap = Map.empty<string, Value>
let mutable funcMap = Map.empty<string, string * terminal list>

let initVarMap() = Map.empty<string, Value>
let initFuncMap() = Map.empty<string, string * terminal list>
let strToList s = [for c in s -> c]
let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c
let isLetter c = Char.IsLetter c
let intVal c = int(Char.GetNumericValue c)

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

let lexer input =
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::'*'::tail | '^'::tail -> Pow :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '%'::tail -> Mod :: scan tail
        | '('::tail -> Lbr :: scan tail
        | ')'::tail -> Rbr :: scan tail
        | '='::tail -> Eql :: scan tail
        | ','::tail -> Comma :: scan tail
        | _ when startsWith "cos" input -> Cos :: scan (drop 3 input)
        | _ when startsWith "sin" input -> Sin :: scan (drop 3 input)
        | _ when startsWith "tan" input -> Tan :: scan (drop 3 input)
        | _ when startsWith "exp" input -> Exp :: scan (drop 3 input)
        | _ when startsWith "log" input -> Log :: scan (drop 3 input)
        | _ when startsWith "integral" input -> Integral :: scan (drop 8 input)
        | c :: tail when isBlank c -> scan tail
        | c :: tail when isLetter c -> let remInput, varName = scanStr(tail, string c)
                                       Var varName :: scan remInput
        | c :: tail when isDigit c -> let remInput, n = scanInt(tail, intVal c)
                                      let n, remInput = match remInput with
                                                          | '.'::tail -> let remInput, decimal = scanFloat(tail, 0., 1.)
                                                                         Num(Float(decimal+float n)), remInput
                                                          | _ -> Num(Int n), remInput
                                      if remInput.Length > 0 && isLetter remInput.Head then (n :: Mul :: scan remInput) else (n :: scan remInput) // allows implicit multiplication of variables
        | _ -> raise (LexerError(List.head input))
    scan (strToList input)

// Grammar in BNF:
// <S>        ::= <Var> "=" <E> | <Var> "(" <Var> ")" "=" <E> | <E>
// <E>        ::= <T> <EOpt>
// <EOpt>     ::= "+" <T> <EOpt> | "-" <T> <EOpt> | "%" <T> <EOpt> | <empty>
// <T>        ::= <F> <TOpt>
// <TOpt>     ::= "*" <F> <TOpt> | "/" <F> <TOpt> | <empty>
// <F>        ::== <NR> <FOpt>
// <FOpt>     ::= "^"|"**" <F> <FOpt> | <empty>
// <NR>       ::= <Num> | "-"<Num> | "-"<Var> | <Var> | <Var> "(" <E> ")" | "(" <E> ")"

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
        | Add :: tail | Sub :: tail | Mod :: tail -> (T >> EOpt) tail
        | _ -> tList
    and T tList = (F >> TOpt) tList
    and TOpt tList =
        match tList with
        | Mul :: tail | Div :: tail -> (F >> TOpt) tail
        | _ -> tList
    and F tList = (NR >> FOpt) tList
    and FOpt tList =
        match tList with
        | Pow :: tail -> (F >> FOpt) tail
        | _ -> tList
    and NR tList =
        match tList with
        | Num _ :: tail | Sub :: Num _ :: tail -> tail
        | Cos :: Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
        | Sin :: Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
        | Tan :: Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
        | Exp :: Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
        | Log :: Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
        | Var v :: Lbr :: tail -> match E tail with | Rbr :: tail -> (if funcMap.ContainsKey(v) then tail else raise (VarUndefined(v))) | _ -> raise ParserError
        | Var v :: tail | Sub :: Var v :: tail -> if varMap.ContainsKey(v) then tail else raise (VarUndefined(v))
        | Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
        | _ -> raise ParserError
    S tList

let toFloat = function | Int v -> float v | Float v -> v
let add a b = match a, b with | Int a, Int b -> Int(a + b) | _ -> Float(toFloat a + toFloat b)
let sub a b = match a, b with | Int a, Int b -> Int(a - b) | _ -> Float(toFloat a - toFloat b)
let mul a b = match a, b with | Int a, Int b -> Int(a * b) | _ -> Float(toFloat a * toFloat b)
let div a b = match a, b with | Int a, Int b when b <> 0 -> Int(a / b) | _ -> Float(toFloat a / toFloat b)
let pow a b = match a, b with | Int a, Int b -> Int(pown a b)  | _ -> Float(toFloat a ** toFloat b)
let modulus a b = match a, b with | Int a, Int b -> Int(a % b) | _ -> Float(toFloat a % toFloat b)


let parseAndEval tList =
    let rec S tList =
        match tList with
        | Var varName :: Eql :: tail ->
            let tList, value = E tail
            varMap <- Map.add varName value varMap
            (tList, value)
        | Var funcName :: Lbr :: Var n :: Rbr :: Eql :: tail ->
            funcMap <- Map.add funcName (n,tail) funcMap
            //debug
            printfn "funcMap: %A" funcMap
            //endDebug
            (tList, Int 0) // got to return something
        | _ -> E tList
    and E tList = (T >> EOpt) tList
    and EOpt (tList, value) =
        match tList with
        // | Add :: tail -> EOpt (T tail ||> fun tLst tVal -> (tLst, add value tVal))
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
        | _ -> (tList, value)
    and NR tList =
        match tList with
        | Num value :: tail -> (tail, value)
        | Sub :: Num value :: tail -> (tail, sub (Int 0) value)
        | Cos :: Lbr :: tail ->
            let tList', num = E tail
            match tList' with
            | Rbr :: tail -> (tail, Float (System.Math.Cos(toFloat num)))
            | _ -> raise ParserError
        | Sin :: Lbr :: tail ->
            let tList', num = E tail
            match tList' with
            | Rbr :: tail -> (tail, Float (System.Math.Sin(toFloat num)))
            | _ -> raise ParserError
        | Tan :: Lbr :: tail ->
            let tList', num = E tail
            match tList' with
            | Rbr :: tail -> (tail, Float (System.Math.Tan(toFloat num)))
            | _ -> raise ParserError
        | Exp :: Lbr :: tail ->
            let tList', num = E tail
            match tList' with
            | Rbr :: tail -> (tail, Float (System.Math.Exp(toFloat num)))
            | _ -> raise ParserError
        | Log :: Lbr :: tail ->
            let tList', num = E tail
            match tList' with
            | Rbr :: tail -> (tail, Float (System.Math.Log(toFloat num)))
            | _ -> raise ParserError
        | Var funcName :: Lbr :: tail -> let tLst, n = E tail
                                         match tLst with
                                         | Rbr :: tail ->
                                             let argName, func = Map.find funcName funcMap
                                             let origMap = varMap
                                             varMap <- Map.add argName n varMap
                                             let _,value = E func
                                             varMap <- origMap
                                             (tail, value)
                                         | _ -> raise ParserError
        | Var varName :: tail -> (tail, Map.find varName varMap)
        | Sub :: Var varName :: tail -> (tail, sub (Int 0) (Map.find varName varMap))
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
    S tList

let rec printTokenList (lst:list<terminal>) : list<string> =
    match lst with
    | head::tail -> printf $"{head.ToString()} "; printTokenList tail
    | [] -> printfn "EOL"; []

let main (input:string, vM, fM)  =
    varMap <- vM; funcMap <- fM
    try
        let tokenList = lexer input
        printTokenList tokenList |> ignore
        //debug
        //let parsedTokens = parser tokenList
        //printfn "Tokens passed to parseAndEval: %A" parsedTokens
        //parser tokenList |> ignore
        let _, result = parseAndEval tokenList
        $"Result = {result}", varMap, funcMap
    with
        | LexerError(c) -> $"Lexer Error, invalid token {c}", varMap, funcMap
        | ParserError -> "Error parsing", varMap, funcMap
        | VarUndefined(v) -> $"Variable {v} is not defined", varMap, funcMap
        | :? OverflowException -> "Overflow error, exceeded max value for int32", varMap, funcMap

let plot (input:string, minX:string, maxX:string, vM, fM)  =
    varMap <- vM; funcMap <- fM
    let tokenList = lexer input
    let minX, maxX = toFloat (snd (lexer minX |> parseAndEval)), toFloat (snd (lexer maxX |> parseAndEval))  // parses minX and maxX as numbers
    let xVals = [for i in 0 .. 99 -> minX + (float i * (maxX-minX)/99.)] // creates 100 x values to plot over the x range
    match tokenList with
        | Var fn :: Lbr :: Var _ :: Rbr :: Eql :: _ ->  // use parser to evaluate function at points by calling e.g. y(2)
            ( [for x in xVals -> (float x, toFloat(snd(parseAndEval([Var fn; Lbr; Num(Float(x)); Rbr]))))], varMap, funcMap)
        | _ -> ([], varMap, funcMap)
