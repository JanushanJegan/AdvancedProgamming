open System
open Microsoft.FSharp.Core.Operators.Checked

type Value = | Int of int | Float of float
and terminal = Add | Sub | Mul | Div | Pow | Mod | Lbr | Rbr | Eql | Num of Value | Var of string

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
        | c :: tail when isBlank c -> scan tail
        | c :: tail when isLetter c -> let remInput, varName = scanStr(tail, string c)
                                       Var varName :: scan remInput
        | c :: tail when isDigit c -> let remInput, n = scanInt(tail, intVal c)
                                      match remInput with
                                      | '.'::tail -> let remInput, decimal = scanFloat(tail, 0., 1.)
                                                     Num(Float(decimal+float n)) :: scan remInput
                                      | _ -> Num(Int n) :: scan remInput
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
            varMap <- Map.add v (Int 0) varMap // temp add Var v to map so function expression can parse
            let tList = E tail
            varMap <- origMap
            tList
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
            (tList, Int 0)
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
        | _ -> raise ParserError
    S tList

let rec printTokenList (lst:list<terminal>) : list<string> =
    match lst with
    | head::tail -> printf $"{head.ToString()} "; printTokenList tail
    | [] -> printfn "EOL"; []

let getInputString() = printfn "Enter an expression: "; Console.ReadLine()

[<EntryPoint>]
let main _  =
    let rec interpret _ =
        try
            let tokenList = lexer(getInputString())
            printTokenList tokenList |> ignore
            parser tokenList |> ignore
            let _, result = parseAndEval tokenList
            printfn $"Result = {result}"
        with
            | LexerError(c) -> printfn $"Lexer Error, invalid token {c}"
            | ParserError -> printfn "Error parsing"
            | VarUndefined(v) -> printfn $"Variable {v} is not defined"
            | :? OverflowException -> printfn $"Overflow error, exceeded max value for int32"
        interpret ()
    interpret()
