open System
open Microsoft.FSharp.Core.Operators.Checked

type Value = | Int of int | Float of float
and terminal = Add | Sub | Mul | Div | Pow | Mod | Lbr | Rbr | Eql | Cos | Sin | Tan | Exp | Ent | Log | Num of Value | Var of string

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

// let startsWith element = function
//     | e::_ when e = element -> true
//     | _ -> false
// let startsWith = function
//     | e::_ -> e
//     | _ -> false

let lexNumber c tail =
    let remInput, n = scanInt(tail, intVal c)
    match remInput with
    | '.'::tail -> let remInput, decimal = scanFloat(tail, 0., 1.)
                   Num(Float(decimal+float n)), remInput
    | _ -> Num(Int n), remInput



let lexer input =
    let rec scan input =
        match input with
        | [] -> []
        | '-'::c :: tail when isDigit c -> let n, remInput = lexNumber c tail
                                           let n = match n with | Num (Int n) -> Num (Int(0 - n)) | Num (Float n) -> Num(Float(0.-n))
                                           if remInput.Length > 0 && isLetter remInput.Head && remInput.Head<>'E' then (Add :: n :: Mul :: scan remInput) else (Add :: n :: scan remInput) // allows implicit multiplication of variables
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::'*'::tail | '^'::tail -> Pow :: scan tail
        | 'E'::tail -> Ent :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '%'::tail -> Mod :: scan tail
        | '('::tail -> Lbr :: scan tail
        | ')'::tail -> Rbr :: scan tail
        | '='::tail -> Eql :: scan tail
        | _ when startsWith "cos" input -> Cos :: scan (drop 3 input)
        | _ when startsWith "sin" input -> Sin :: scan (drop 3 input)
        | _ when startsWith "tan" input -> Tan :: scan (drop 3 input)
        | _ when startsWith "exp" input -> Exp :: scan (drop 3 input)
        | _ when startsWith "log" input -> Log :: scan (drop 3 input)
        | c :: tail when isBlank c -> scan tail
        | c :: tail when isLetter c -> let remInput, varName = scanStr(tail, string c)
                                       Var varName :: scan remInput
        | c :: tail when isDigit c -> let n, remInput = lexNumber c tail
                                      if remInput.Length > 0 && isLetter remInput.Head && remInput.Head<>'E' then (n :: Mul :: scan remInput) else (n :: scan remInput) // allows implicit multiplication of variables
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
// <NR>       ::= <Num> | "-"<Num> | "-"<Var> | <Var> | <Var> "(" <E> ")" | <Cos|Sin|Tan|Exp|Log> "(" <E> ")" | "(" <E> ")"

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
        | Num _ :: tail | Sub :: Num _ :: tail -> tail
        | (Cos|Sin|Tan|Exp|Log) :: Lbr :: tail -> match E tail with | Rbr :: tail -> tail | _ -> raise ParserError
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
                         FOpt (tLst, mul value (pow (Int 10) tVal))
        | _ -> (tList, value)
    and NR tList =
        match tList with
        | Num value :: tail -> (tail, value)
        | Sub :: Num value :: tail -> (tail, sub (Int 0) value)
        | Cos|Sin|Tan|Exp|Log as f :: Lbr :: tail ->
            let tList', num = E tail
            match tList' with
            | Rbr :: tail -> match f with
                             | Cos -> (tail, Float (Math.Cos(toFloat num)))
                             | Sin -> (tail, Float (Math.Sin(toFloat num)))
                             | Tan -> (tail, Float (Math.Tan(toFloat num)))
                             | Log -> (tail, Float (Math.Log(toFloat num)))
                             | Exp -> (tail, Float (Math.Exp(toFloat num)))
            | _ -> raise ParserError
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
        | Sub :: Var varName :: tail -> (tail, sub (Int 0) (Map.find varName varMap))
        | Lbr :: tail -> let tLst, value = E tail
                         match tLst with
                          | Rbr :: tail -> (tail, value)
                          | _ -> raise ParserError
        | _ -> raise ParserError
    S tList

let unlexer tList =
    let rec toString = function
        | [] -> ""
        | Num(Int n) :: Mul :: Var v :: tail -> string n + v + toString tail // 3*x => 3x
        | Num(Float n) :: Mul :: Var v :: tail -> string n + v + toString tail
        | Num(Int n) :: tail -> string n + toString tail
        | Num(Float f) :: tail -> string f + toString tail
        | Var v :: tail -> v + toString tail
        | Add :: Num (Int n) :: tail when n<0 -> " - " + toString (Num (Int -n)::tail) // + -3 -> - 3
        | Add :: Num (Float n) :: tail when n<0 -> " - " + toString (Num (Float -n)::tail)
        | Add :: tail -> " + " + toString tail
        | Sub :: tail -> " - " + toString tail
        | Mul :: tail -> " * " + toString tail
        | Div :: tail -> " / " + toString tail
        | Pow :: tail -> "^" + toString tail
        | Lbr :: tail -> "(" + toString tail
        | Rbr :: tail -> ")" + toString tail
        | Cos :: tail -> "cos" + toString tail
        | Sin :: tail -> "sin" + toString tail
        | Tan :: tail -> "tan" + toString tail
        | Exp :: tail -> "e" + toString tail
        | Log :: tail -> "log" + toString tail
        | _ -> raise ParserError
    toString tList

/// extract sub-expression contained within brackets e.g. 3+3(x+3)^2) * 3 -> 3+3(x+3)^2
let rec getExprFromBrackets tList depth =
    match tList with
    | [] -> ([], [])
    | Rbr :: tail when depth = 1 -> ([], tail)
    | Rbr :: tail -> let inner, remaining = getExprFromBrackets tail (depth - 1)
                     (Rbr :: inner, remaining)
    | Lbr :: tail -> let inner, remaining = getExprFromBrackets tail (depth + 1)
                     (Lbr :: inner, remaining)
    | head :: tail -> let inner, remaining = getExprFromBrackets tail depth
                      (head :: inner, remaining)
let containsAny elements list = List.exists (fun x -> List.contains x list) elements
let startsWithAny elements = function | a::_ when List.contains a elements -> true | _ -> false

let getNextTerm tList =
    let rec loop acc tList depth =
        match tList with
        | [] -> (List.rev acc, [])
        | Add|Sub as o::tail when depth=1 -> (List.rev acc, o::tail)
        | Rbr :: tail ->  loop (Rbr::acc) tail (depth-1)
        | Lbr :: tail -> loop (Lbr::acc) tail (depth+1)
        | head::tail -> loop (head::acc) tail depth
    loop [] tList 1
let getLastTerm lst =
    // printfn $"getLastExpression: %A{lst}"
    let rec loop acc tList =
        // printfn $"calling getExpression on: %A{tList}"
        let expr, rest = getNextTerm tList
        // printfn $"Expr: %A{expr}, rest: %A{rest}"
        match rest with
        | [] -> expr, acc
        | o::rest -> loop (acc@expr@[o]) rest
    loop [] lst

/// split list into 2 parts around the first occurrence of an operator in the current level e.g. * -> (3*x+5) * (2x+3) -> (3*x+5), (2x+3)
let rec splitAtOperator op tList depth =
    match tList with
    | [] -> ([], [])
    | head :: tail when head = op && depth = 0 -> ([], tail)
    | Lbr :: tail ->
        let left, right = splitAtOperator op tail (depth + 1)
        (Lbr :: left, right)
    | Rbr :: tail ->
        let left, right = splitAtOperator op tail (depth - 1)
        (Rbr :: left, right)
    | head :: tail ->
        let left, right = splitAtOperator op tail depth
        (head :: left, right)
/// gets current level of expression, ignoring nested expressions - 3x + (2x*3) + 7 -> 3x * () * 7
let rec getCurrentLevel tList depth =
        match tList with
        | [] -> []
        | Rbr :: tail ->  Rbr :: getCurrentLevel tail (depth - 1)
        | Lbr :: tail -> Lbr :: getCurrentLevel tail (depth + 1)
        | head :: tail when depth = 1 -> head :: (getCurrentLevel tail 1)
        | _ :: tail -> (getCurrentLevel tail depth)
// let containsAny elements = function | a::_ when List.contains a elements -> true | _ -> false
let rec simplify terminals =
    let combineLikeTerms tList =
        let rec convert acc tail =
            match tail with
            | Num n :: Mul :: Var v :: Pow :: Num p :: tail when (tail.Length=0 || tail.Head=Add) ->  convert (acc@[Num n; Mul; Var v; Pow; Num p]) tail
            | Var v :: Pow :: Num p :: tail when (tail.Length=0 || tail.Head=Add) -> convert (acc@[Num (Int 1); Mul; Var v; Pow; Num p]) tail
            | Num n :: Mul :: Var v :: tail when (tail.Length=0 || tail.Head=Add) -> convert (acc@[Num n; Mul; Var v; Pow; Num (Int 1)]) tail
            | Num n :: tail when (tail.Length=0 || tail.Head=Add) -> convert (acc@[Num n; Mul; Var "x"; Pow; Num (Int 0)]) tail
            | Add as o::tail -> convert (acc@[o]) tail
            // | (Add|Lbr as a)::(Num _|Var _ as b)::tail -> convert (acc@[a]) (b::tail)

            | _ -> (acc, tail)

        let converted, rest = convert [] tList
        printfn $"Converted: %A{unlexer converted}, The rest: {unlexer rest}"
        let mutable termMap = Map.empty<terminal list, Value>
        let rec loop tList =
            match tList with
            | Num n :: Mul :: Var v :: Pow :: Num p :: tail ->
                let term = [Var v; Pow; Num p]
                termMap <- Map.add term (add n (if termMap.ContainsKey term then termMap[term] else (Int 0))) termMap
                loop tail
            | _::tail -> loop tail
            | [] -> ()
        loop converted
        printfn $"termmap: {termMap}"
        (termMap
        |> Map.toList
        |> List.sortByDescending (fun (term, _) -> match term with | Var _ :: Pow :: Num p :: _ -> toFloat p) // sort by power
        |> List.fold (fun acc (term, coeff) -> acc @ simplify (Num coeff :: Mul ::term) @ [Add] ) []
        // |> List.fold (fun acc (term, coeff) ->
        //     match term with
        //     // | _ -> acc @ [Num coeff; Mul] @ term @ [Add]
        //     | _ when toFloat coeff=0 -> []
        //     | [Var v; Pow; Num p] when p=(Int 1)-> acc @ [Num coeff; Mul] @ [Var v] @ [Add]
        //     | [Var _; Pow; Num p] when p=(Int 0)-> acc @ [Num coeff; Add]
        //     | _ -> acc @ [Num coeff; Mul] @ term @ [Add]
        //  ) []
            // acc @ [Num coeff; Mul] @ term @ [Add]) []
        , rest)



    // let getExpression lst =
    //     let rec loop acc = function
    //         | [] -> (List.rev acc, [])
    //         | (Add|Sub)::tail -> (List.rev acc, tail)
    //         | head::tail -> loop (head::acc) tail
    //     loop [] lst

    let endOfTerm = function // checks if tList starts with a */^
        | (Mul|Div|Pow) :: _ -> false
        | _ -> true


    let splitByElement lst =
        let rec split acc current = function
            | [] -> List.rev (List.rev current :: acc)
            // | Sub::xs -> split (List.rev current :: acc) [] (Sub::xs)
            // | Sub::tail -> split (List.rev current :: acc) [] tail
            | (Add|Sub)::tail when current.Length>0 -> split (List.rev current :: acc) [] tail
            // | (Lbr|Rbr)::tail -> split acc current tail
            // | Lbr::tail -> let a, tail = extractExprFrBrackets tail 1
                           // split acc current tail
            | x::tail -> split acc (x::current) tail

        split [] [] lst
    ///  expands out (a * b * ... ) * c
    let rec expandBracket first second = // (a * b * ... ) * c
            match first with
            | [] -> []
            // | term::rest -> (term @ [Mul; Lbr] @ second @ [Rbr]) @ [Add] @ distributeTerms rest second
            | term::rest -> (term @ [Mul; Lbr] @ second @ [Rbr]) @ (if rest.IsEmpty then [] else [Add]) @ expandBracket rest second
            // | term::rest -> (term @ [Mul] @ second) @ [Add] @ distributeTerms rest second
    ///  expands out (a * b * ...) * (c * d * ...)
    let rec expandBrackets first second =
        printfn $"Expanding %A{first} * %A{second}"
        match first with
        | [] -> []
        | term::rest -> expandBracket second term @ (if rest.IsEmpty then [] else [Add]) @ expandBrackets rest second
    /// checks the next term is in the form ax^n, x^n or a
    let isPureTerm = function
        | Num _ :: t | Num _ :: Mul :: Var _ :: t | Num _ :: Mul :: Var _ :: Pow :: Num _ :: t when startsWithAny [Add; Rbr] t || t.IsEmpty -> true
        | _ -> false

    // let getExpression lst =
    //         let rec loop acc = function
    //             | [] -> (List.rev acc, [])
    //             | (Add|Sub as o)::tail -> (List.rev acc, o::tail)
    //             | head::tail -> loop (head::acc) tail
    //         loop [] lst
    let rec simplifyExpr terminals acc =
        printfn $"simplifyExpr - %A{terminals} - %A{acc}  | %A{unlexer terminals} - %A{unlexer acc}"

        match terminals with

        | [] -> printfn $"Done: %A{acc}"; acc
        // | Add::Rbr::rest -> simplifyExpr rest acc
        | Sub::Lbr::rest -> printfn "Doing -("; simplifyExpr (acc@[Add; Num(Int -1); Mul; Lbr]@rest) []
        | (Add|Sub) :: rest when rest=[] -> printfn "removing stray +/-"; simplifyExpr rest acc
        // | (Add|Sub as o1) :: (Add|Sub as o2) :: rest  when o1=o2 -> simplifyExpr (acc@o1::rest) [] // ++ -> +  -- -> +
        // | (Add|Sub as o1) :: (Add|Sub as o2) :: rest  when o1<>o2 -> simplifyExpr (acc@Sub::rest) [] // +- -> -
        | Add|Sub as o1 :: (Add|Sub as o2) :: rest  when o1=o2 -> simplifyExpr (Add::rest) acc // ++ -> +  -- -> +
        | Add|Sub as o1 :: (Add|Sub as o2) :: rest  when o1<>o2 -> simplifyExpr (Sub::rest) acc // +- -> -
        | Num (Int 0) :: Add :: rest -> simplifyExpr rest acc // 0 +
        | (Add | Sub) :: Num (Int 0) :: rest when endOfTerm rest -> simplifyExpr rest acc // - 0, + 0 when at the end of an expression
        // | (Add | Sub) :: Num (Int 0) :: rest-> simplifyExpr rest acc // - 0, + 0 when at the end of an expression
        | Num a :: Add :: Num b :: rest when endOfTerm rest && endOfTerm (List.rev acc) -> simplifyExpr (acc@[Num(add a b)]@rest) [] // x+x = 2x
        | a :: Add :: b :: rest when a=b && endOfTerm rest && endOfTerm (List.rev acc) -> simplifyExpr (acc@[Lbr; Num (Int 2); Mul; a; Rbr]@rest) [] // x+x = 2x
        | Num n :: Mul :: a :: Add :: b :: rest when a=b && endOfTerm rest -> simplifyExpr (acc@[Lbr; Num (add n (Int 1)); Mul; a; Rbr]@rest) [] // ax+x = (a+1)x

        | Num (Int 1) :: Mul :: rest | Mul :: Num (Int 1) :: rest-> simplifyExpr rest acc // 1*x or x*1 -> x

        | Num a :: Mul :: Num b :: rest -> simplifyExpr (acc@[Num(mul a b)]@rest) [] // n1*n2
        | Var v1 :: Mul :: Var v2 :: Pow :: Num n :: rest when v1=v2 -> simplifyExpr (acc@[Var v1; Pow; Num (add n (Int 1))]@rest) [] // x*x^n -> x^(n+1)
        | Var v1 :: Pow :: Num a :: Mul :: Var v2 :: Pow :: Num b :: rest when v1=v2 -> simplifyExpr (acc@[Var v1; Pow; Num (add a b)]@rest) [] // x^a * x^b -> x^(a*b)
        | a :: Mul :: b :: rest when a=b -> simplifyExpr (acc@[a; Pow; Num (Int 2)]@rest) [] // x*x -> x^2
        | Pow :: Num a :: Pow :: Num b :: rest -> simplifyExpr (acc@[Pow; Num (mul a b)]@rest) [] // x^a^b = x^(a+b)
        | Pow :: Num (Int 1) :: rest -> simplifyExpr rest acc // x^1 -> x
        | Var _ :: Pow :: Num (Int 0) :: rest -> simplifyExpr (acc@[Num (Int 1)]@rest) [] // x^0 -> 1

        | Num a :: Pow :: Num b :: rest -> simplifyExpr (acc@[Num (pow a b)]@rest) [] // a^b

        | Num a :: Mul :: Var v :: Mul :: Num b :: rest -> simplifyExpr (acc @ [Num (mul a b); Mul; Var v]@rest) [] // a*x*b = (a*b)*x
        | a :: Mul :: b :: Mul :: c :: rest when a=c -> simplifyExpr (acc @ [b; Mul; a; Mul; c]@rest) [] // x * 2 * x -> 2 * x * x


        | Num (Int 0) :: Mul :: Lbr :: rest -> let a, remaining = getExprFromBrackets rest 1 // 0*()
                                               printfn $"Doing 0x() - $%A{unlexer a} | $%A{unlexer remaining}"
                                               simplifyExpr (acc@(Num (Int 0)::remaining)) []
        | Num (Int 0) :: Mul :: rest -> let toRemove, remaining = getNextTerm rest
                                        printfn $"Doing 0x() 2 - $%A{unlexer toRemove} | $%A{unlexer remaining}"
                                        simplifyExpr (acc@(Num (Int 0)::remaining)) []
        | (Sin|Cos|Tan|Log|Exp as func) :: Lbr :: rest ->
            let inner, remaining = getExprFromBrackets rest 1
            let simplifiedInner = simplify inner  // Keep brackets around function arguments
            // simplifyExpr (func :: Lbr :: simplifiedInner @ [Rbr] @ remaining) acc
            simplifyExpr remaining (acc@[func;Lbr]@simplifiedInner@[Rbr])

        | Lbr :: rest ->
            printfn $"Processing %A{unlexer ([Lbr]@rest)}"
            let inner, remaining = getExprFromBrackets rest 1
            printfn $"Inner %A{unlexer inner}, rest %A{unlexer remaining}"
            let simplifiedInner = simplify inner
            let cond = (containsAny [Add; Sub] simplifiedInner) && (rest.Head <> Lbr || (startsWithAny [Div; Mul; Pow] remaining))
            let cond1 = (List.contains Add simplifiedInner || List.contains Sub simplifiedInner) && (rest.Head <> Lbr || (remaining.Length > 0 && (remaining.Head=Div || remaining.Head=Mul || remaining.Head=Pow)))
            printfn $"Simplified %A{unlexer inner} -> %A{unlexer simplifiedInner} - %A{unlexer remaining} - {cond} {cond1} - {rest.Head} - %A{acc} - original: %A{unlexer ([Lbr]@rest)}"
            // simplifyExpr (simplifiedInner @ remaining) acc
            // match remaining with
            // | Pow::_ when ((simplify simplifiedInner)=simplifiedInner && List.contains Add simplifiedInner || List.contains Sub simplifiedInner) -> printfn $"Fully simplified, {remaining} remaining"; simplifyExpr remaining (acc@[Lbr]@simplifiedInner@[Rbr])
            // | _ -> simplifyExpr (simplifiedInner @ remaining) acc
            match simplifiedInner with
            // | _ when rest.Head <> Lbr && (acc.Length = 0 || ((List.last acc)=Add || (List.last acc)=Sub)) && (remaining.Length = 0 || (remaining[0]=Add || remaining[0]=Sub)) -> printfn "Doing this"; simplifyExpr (acc @ simplifiedInner @ remaining) []
            | _ when ((acc.Length = 0 || (startsWithAny [Add; Sub] (List.rev acc))) && (not (startsWithAny [Div; Mul; Pow] remaining))) -> printfn "Doing this"; simplifyExpr (acc @ simplifiedInner @ remaining) []

            | _ when (containsAny [Add; Sub; Div] simplifiedInner) && (rest.Head <> Lbr || (startsWithAny [Div; Mul] remaining) || (startsWithAny [Div; Mul] (List.rev acc))) -> printfn $"Fully simplified, sI: %A{simplifiedInner}, %A{remaining} remaining - %A{unlexer remaining}"; simplifyExpr remaining (acc@[Lbr]@simplifiedInner@[Rbr])
            | _ -> simplifyExpr (acc @ simplifiedInner @ remaining) []


        // | Mul :: b :: rest when ((List.last acc)=Rbr || (b=Lbr && (match rest with _ :: Rbr :: _ -> false | _ -> true))) -> // Product Rule - f(x) * g(x) -> f'(x) * g(x) + f(x) * g'(x)
        | Mul :: b :: rest when ((List.last acc)=Rbr || b=Lbr) -> // expand out bracket * bracket
            printfn $"Doing Mul rule on %A{unlexer terminals}, before %A{unlexer acc}"
            let (leftExpr, before), (rightExpr, after) = (getLastTerm acc), (getNextTerm (b::rest))
            // let rightExpr, rest = extractExprFrBrackets rightExpr 1
            // let after = rest@after
            printfn $"before: %A{unlexer before}, leftExpr: %A{unlexer leftExpr}, rightExpr: %A{unlexer rightExpr}, after: %A{unlexer after}"
            let leftExpr = simplify leftExpr
            printfn $"Simplified leftExpr: %A{leftExpr}"
            let rightExpr = simplify rightExpr
            printfn $"Simplified rightExpr: %A{rightExpr}"
            match (leftExpr, rightExpr) with
            // one of the brackets contains multiple terms so needs expanding, and neither brackets are raised to a power
            | _ when (containsAny [Add; Sub] leftExpr || containsAny [Add; Sub] rightExpr) && (match (List.rev leftExpr) with | Num _::Pow::Rbr::_ -> false | _ -> true && match (List.rev rightExpr) with | Num _::Pow::Rbr::_ -> false | _ -> true) ->
                // let leftExpr = simplify leftExpr
                // printfn $"Simplified leftExpr: %A{leftExpr}"
                // let rightExpr = simplify rightExpr
                // printfn $"Simplified rightExpr: %A{rightExpr}" 2x^3 + (2x + 3) * 3x^2
                let expansion = expandBrackets (splitByElement leftExpr) (splitByElement rightExpr)
                printfn $"expanded to %A{unlexer expansion}"
                printfn $"Result: %A{unlexer (before@expansion@after)}"
                simplifyExpr (before@Lbr::expansion@Rbr::after) []
            // | (_, Lbr::Num n::Rbr::_) -> printfn "Doing something*number"; simplifyExpr (before@rightExpr@[Mul]@leftExpr@after) [] // simple number*number, no need to expand, might not be needed
            | (_, Lbr::Num n::[ Rbr ]) | (_, [Num n]) -> printfn "Doing something*number"; simplifyExpr (before@[Num n]@[Mul]@leftExpr@after) [] // something like 3x^2 * 3, swap round so numbers can becombined
            | _ -> printfn "Doing last one"; simplifyExpr (b::rest) (acc @ [Mul]) // simple term * term, no need to expand
            // let rightExpr, rest = extractExprFrBrackets rightExpr 1
            // printfn $"rightExpr: %A{terminalListToString rightExpr}, rest: %A{terminalListToString rest}"
            // let rightExpr, rest = getExpression rightExpr
            // printfn $"rightExpr: %A{terminalListToString rightExpr}, rest: %A{terminalListToString rest}"
            // let leftExpr = getLastExpression leftExpr
            // printfn $"leftExpr: %A{terminalListToString leftExpr}"
        | a :: Div :: b :: rest when (fst (getNextTerm (b::rest)))=(fst (getLastTerm (acc@[a]))) ->
            printfn "Cancelling fraction"
        // | a :: Div :: b :: rest ->
            let _, start = getLastTerm (acc@[a])
            let _, rest = getNextTerm (b::rest)
            simplifyExpr (start@[Num (Int 1)]@rest) []

        | (Pow|Mul|Div as o) :: Num n :: rest -> simplifyExpr rest (acc@[o; Num n]) // keeps operators attached to brackets

        // | (Num a :: Mul :: Var v :: Pow :: Num p :: Add :: Num p1 :: rest) as full when isPureTerm (Num p1::rest) ->
        | Add :: b :: rest when isPureTerm ((fst (getLastTerm acc))@[Add]) && isPureTerm (b::rest) ->  // combine terms like 3x^2 + 6x^2
            let lastTerm, before = getLastTerm acc
            let full = lastTerm@Add::b::rest
            printfn $"Combining like terms on {unlexer full}"
            let combinedTerms, rest = combineLikeTerms full
            let combinedTerms = if rest.IsEmpty then combinedTerms[0..combinedTerms.Length-2] else combinedTerms // strip trailing +
            // printfn $"Combined: {terminalListToString combinedTerms}"
            // let combinedTerms = simplify combinedTerms
            printfn $"Combined like terms - {unlexer combinedTerms}, rest: {unlexer rest}"
            simplifyExpr rest (before@combinedTerms)



        // | Var v1 :: Pow :: Num n1 :: Add :: Num b :: Mul :: Var v2 :: Pow :: Num n2 :: rest // x^p + n*x^p -> (n+1)*x^p
        //   when v1 = v2 && n1 = n2 ->
        //     let newCoeff = add (Int 1) b
        //     simplifyExpr (Num newCoeff :: Mul :: Var v1 :: Pow :: Num n1 :: rest) acc
        //
        // // a*x^p + b*x^p -> (a+b)*x^p
        // | Num a :: Mul :: Var v1 :: Pow :: Num n1 :: Add :: Num b :: Mul :: Var v2 :: Pow :: Num n2 :: rest
        //   when v1 = v2 && n1 = n2 ->
        //     let newCoeff = add a b
        //     simplifyExpr (Num newCoeff :: Mul :: Var v1 :: Pow :: Num n1 :: rest) acc
        //
        // // n*x^p + x^p -> (n+1)*x^p
        // | Num a :: Mul :: Var v1 :: Pow :: Num n1 :: Add :: Var v2 :: Pow :: Num n2 :: rest
        //   when v1 = v2 && n1 = n2 ->
        //     let newCoeff = add a (Int 1)
        //     simplifyExpr (Num newCoeff :: Mul :: Var v1 :: Pow :: Num n1 :: rest) acc
        //
        // // x^p + x^p -> 2*x^2
        // | Var v1 :: Pow :: Num n1 :: Add :: Var v2 :: Pow :: Num n2 :: rest
        //   when v1 = v2 && n1 = n2 ->
        //     simplifyExpr (Num (Int 2) :: Mul :: Var v1 :: Pow :: Num n1 :: rest) acc
        //
        // // a*x + b*x -> (a+b)*x
        // | Num a :: Mul :: Var v1 :: Add :: Num b :: Mul :: Var v2 :: rest
        //   when v1 = v2 && acc.Length=0 ->
        //     printfn $"doing a*x + b*x - a: {a}, b: {b}, rest: %A{rest}"
        //     let newCoeff = add a b
        //     simplifyExpr (Num newCoeff :: Mul :: Var v1 :: rest) acc

        | head :: tail -> printfn "Moving on"; simplifyExpr tail (acc @ [head])



    simplifyExpr terminals []

let differentiate tList =
    printfn $"Differentiating %A{unlexer tList}"
    let tList = simplify tList
    printfn $"Simplified to %A{unlexer tList}"
    let tv = "x"



    /// don't do chain rule if bracket is part of * or /, do product rule/quotient rule first instead
    let doChainRule tail = let _, remaining = getExprFromBrackets tail 1
                           match remaining with
                            | (Div|Mul) :: _ -> false
                            | _ -> true
    /// don't bother with product rule if number * something
    let doProductRule lst =
        let leftExpr, rightExpr = splitAtOperator Mul lst 0
        let rightExpr, _ = getNextTerm rightExpr
        let leftExpr, _ = getLastTerm leftExpr
        match (leftExpr, rightExpr) with
        | [Num _],_ | _,[Num _] -> false
        | _ -> true

    let rec diff tList =
        printfn $"Differentiating %A{tList} - %A{unlexer tList}"
        match tList with
        | [] -> []
        | Add|Sub as op :: tail-> op :: diff tail

        | Lbr :: tail when doChainRule tail-> // Chain Rule - (f)^n -> (f)^(n-1) * f'
            let innerExpr, remaining = getExprFromBrackets tail 1
            printfn $"innerExpr: %A{innerExpr}, remaining: %A{remaining}"
            match remaining with
            | Pow :: Num (Int p) :: tail ->
                let newCoeff, newPow = (Int p, Int (p - 1))
                let innerDiff = diff innerExpr
                printfn $"diff innerExpr: %A{innerDiff}"
                printfn $"cr result: %A{Lbr::innerDiff@[Rbr; Mul] @ [Num newCoeff; Mul] @ Lbr :: innerExpr @ [Rbr; Pow; Num newPow]}"
                Lbr::innerDiff@[Rbr; Mul] @ [Num newCoeff; Mul] @ Lbr :: innerExpr @ [Rbr; Pow; Num newPow] @ diff tail
            | _ -> diff innerExpr
        | _ when List.contains Div (getCurrentLevel tList 1) -> // Quotient Rule - f(x) / g(x) -> (f'(x) * g(x) - f(x) * g'(x)) / g(x)^2
            printfn $"Doing quotient rule on %A{unlexer tList}"
            let leftExpr, rightExpr = splitAtOperator Div tList 0
            printfn $"leftExpr: %A{unlexer leftExpr}, rightExpr: %A{unlexer rightExpr}"
            let rightExpr, rest = getNextTerm rightExpr
            printfn $"rightExpr: %A{rightExpr}, rest: %A{rest}"
            let leftExpr, start = getLastTerm leftExpr
            let leftDiff, rightDiff = (diff leftExpr, diff rightExpr)
            printfn $"diff leftExpr: %A{unlexer leftDiff}, diff rightExpr: %A{unlexer rightDiff}"
            let num = [Lbr; Lbr] @ leftDiff @ [Rbr; Mul; Lbr] @ rightExpr @ [Rbr; Rbr; Sub; Lbr; Lbr] @ leftExpr @ [Rbr; Mul; Lbr] @ rightDiff @ [Rbr; Rbr]
            let denom = [Lbr] @ rightExpr @ [Pow; Num (Int 2); Rbr]
            printfn $"Done Quotient rule - %A{num} / %A{denom} - %A{unlexer num} / %A{unlexer denom}"
            start @ [Lbr] @ num @ [Rbr; Div] @ denom @ diff rest
        | _ when List.contains Mul (getCurrentLevel tList 1) && doProductRule tList -> // Product Rule - f(x) * g(x) -> f'(x) * g(x) + f(x) * g'(x)
            printfn $"Doing product rule on %A{unlexer tList}"
            // let expr, rest = extractExprFrBrackets tList 0
            // printfn $"Expr: %A{terminalListToString expr}, rest: %A{terminalListToString rest}"
            // let expr, rest = extractExprFrBrackets tList 1
            // printfn $"Expr: %A{terminalListToString expr}, rest: %A{terminalListToString rest}"
            // let expr, rest = extractExprFrBrackets tList 2
            // printfn $"Expr: %A{terminalListToString expr}, rest: %A{terminalListToString rest}"
            let leftExpr, rightExpr = splitAtOperator Mul tList 0
            printfn $"leftExpr: %A{leftExpr}, rightExpr: %A{rightExpr}"
            let rightExpr, rest = getNextTerm rightExpr
            printfn $"rightExpr: %A{rightExpr}, rest: %A{rest}"
            let leftExpr, start = getLastTerm leftExpr
            printfn $"leftExpr: %A{leftExpr}, start: %A{start}"
            let leftDiff, rightDiff = (diff leftExpr, diff rightExpr)
            printfn $"diff leftExpr: %A{leftDiff}, diff rightExpr: %A{rightDiff}"
            let res = [Lbr] @ leftDiff @ [Rbr; Mul; Lbr] @ rightExpr @ [Rbr; Add; Lbr] @ leftExpr @ [Rbr; Mul; Lbr] @ rightDiff @ [Rbr]
            printfn $"pr result: %A{ res} <- %A{leftExpr}*%A{rightExpr}  %A{unlexer res}"
            start @ [Lbr; Lbr] @ leftDiff @ [Rbr; Mul; Lbr] @ rightExpr @ [Rbr; Add; Lbr] @ leftExpr @ [Rbr; Mul; Lbr] @ rightDiff @ [Rbr; Rbr] @ diff rest

        | Sin|Cos|Tan|Exp|Log as f :: Lbr :: tail -> // built in functions
            printfn $"Doing {f} - %A{tail}"
            let innerExpr, remaining = getExprFromBrackets tail 1
            printfn $"innerExpr: %A{innerExpr}, remaining: %A{remaining}"
            let innerDiff = diff innerExpr
            let funcDiff = match f with
                           | Sin -> [Mul; Cos; Lbr] @ innerExpr @ [Rbr] // x' * cos(x)
                           | Cos -> [Mul; Sub; Sin; Lbr] @ innerExpr @ [Rbr] // x' * -sin(x)
                           | Exp -> [Mul; Exp; Lbr] @ innerExpr @ [Rbr] // x' * exp(x)
                           | Tan ->  Div :: Cos :: Lbr :: innerExpr @ [Rbr; Pow; Num (Int 2)] // x' * sec^2(x) or 1/cos^2
                           | Log ->  Div :: Lbr :: innerExpr @ [Rbr] // x'/x
            printfn $"Diff {f}: %A{funcDiff @ innerExpr @ [Rbr; Mul] @ Lbr::innerDiff@[Rbr]}"
            // funcDiff @ [Mul; Lbr] @ innerDiff@[Rbr] @ diff remaining
            [Lbr] @ innerDiff@[Rbr] @ funcDiff @  diff remaining // x'
        // | Num(Int a) :: Mul :: Var "x" :: Pow :: Num(Int n) :: tail ->  // Power Rule - a * x^n -> a * n *  x^(n-1)
        //     printfn "Doing power rule 1"
        //     [Num (Int (a * n)); Mul; Var "x"; Pow; Num (Int (n - 1))] @ diff tail
        | Var v :: Pow :: Num n :: tail when v=tv -> // Power Rule - x^n -> n * x^(n-1)
            printfn "Doing power rule 2"
            [Num n; Mul; Var v; Pow; Num (sub n (Int 1) ) ] @ diff tail
        | Num n :: Mul :: tail -> Num n :: Mul :: diff tail
        | Num _ :: tail -> [Num (Int 0)] @ diff tail  // number ->
        | Var _ :: Mul :: Num n :: tail -> [Num n] @ diff tail
        | Var v :: tail when v<>tv -> [Num (Int 0)] @ diff tail  // number -> 0
        | Var v :: tail when v=tv -> [Num (Int 1)] @ diff tail  // x^1 -> 1
        | _ -> raise ParserError
    diff tList




let rec printTokenList (lst:list<terminal>) : list<string> =
    match lst with
    | head::tail -> printf $"{head.ToString()} "; printTokenList tail
    | [] -> printfn "EOL"; []

let getInputString() = printfn "Enter an expression: "; Console.ReadLine()








[<EntryPoint>]
let main _  =
    let rec interpret _ =
        // printfn "Differentiating\n"
        // let toDiff = lexer "(2x+x)*(x)"
        // // 5*x^2 -> Num (Int 5) Mul Var "x" Pow Num (Int 2) -> Num (Int 0) Mul Var "x" Pow Num (Int 2) Add Num (Int 5) Mul Num (Int 2) Mul Var "x" Pow Num (Int 1) -> 0*x^2 + 5*2*x^1 -> 10*x
        // printTokenList toDiff
        // let differentiated = differentiate toDiff
        // printTokenList differentiated
        // printfn $"{(terminalListToString differentiated)}"
        // let simplified = simplify differentiated
        // printTokenList simplified
        // printfn $"{(terminalListToString simplified)}"
        // printfn $"(2 * 2 * x + 2 * x) - Should be 6x"
        // printfn "\nDifferentiating"
        // let toDiff = lexer "x*x*x"
        // // 5*x^2 -> Num (Int 5) Mul Var "x" Pow Num (Int 2) -> Num (Int 0) Mul Var "x" Pow Num (Int 2) Add Num (Int 5) Mul Num (Int 2) Mul Var "x" Pow Num (Int 1) -> 0*x^2 + 5*2*x^1 -> 10*x
        // printTokenList toDiff
        // let differentiated = differentiate toDiff
        // printTokenList differentiated
        // printfn $"{(terminalListToString differentiated)}"
        // let simplified = simplify differentiated
        // printTokenList simplified
        // printfn $"{(terminalListToString simplified)}"
        // printfn $"x^2 + x * 2 * x) - Should be 3x^2"
        //
        // printfn "\n2 / x^2"
        // let toDiff = lexer "2 / x^2"
        // // 5*x^2 -> Num (Int 5) Mul Var "x" Pow Num (Int 2) -> Num (Int 0) Mul Var "x" Pow Num (Int 2) Add Num (Int 5) Mul Num (Int 2) Mul Var "x" Pow Num (Int 1) -> 0*x^2 + 5*2*x^1 -> 10*x
        // printTokenList toDiff
        // let differentiated = differentiate toDiff
        // printTokenList differentiated
        // printfn $"{(terminalListToString differentiated)}"
        // let simplified = simplify differentiated
        // printTokenList simplified
        // printfn $"{(terminalListToString simplified)}"
        // printfn $"0 - 4 * x / x^4 - Should be -4/(x^3)"
        //
        // printfn "\nDifferentiate (3x+5)^3 + cos(2x)"
        // let toDiff = lexer "(3x+5)^3 + cos(2x)"
        // // (3x+5)^3 + cos(2x) -> 9(3x+5)^2 - 2sin(2x)
        // printTokenList toDiff
        // let differentiated = differentiate toDiff
        // printTokenList differentiated
        // printfn $"{(terminalListToString differentiated)}"
        // let simplified = simplify differentiated
        // printTokenList simplified
        // printfn $"{(terminalListToString simplified)}"
        // printfn $"9 * (3 * x + 5)^2 + 2 *  - sin(2 * x) - Should be "
        //
        // printfn "\nDifferentiate (3x+5)/(x^2-4)"
        // let toDiff = lexer "(3x+5)/(x^2-4)"
        // // (3x+5)^3 + cos(2x) -> 9(3x+5)^2 - 2sin(2x)
        // printTokenList toDiff
        // let differentiated = differentiate toDiff
        // printTokenList differentiated
        // printfn $"{(terminalListToString differentiated)}"
        // let simplified = simplify differentiated
        // printTokenList simplified
        // printfn $"{(terminalListToString simplified)}"
        // printfn $"3 * (x^2 - 4) - (3 * x + 5) * 2 * x - 0 / (x^2 - 4)^2 - Should be (-3x^2 - 10x - 12) / (x^2 - 4)^2"
        //
        // printfn "Simplify: \n"
        // let toSimplify = lexer "(2x+x)*(x)"
        //
        // printfn $"%A{toSimplify} - {terminalListToString toSimplify}"
        // let simplified = simplify toSimplify
        // printTokenList simplified
        // printfn $"{(terminalListToString simplified)}"
        //
        // let getExpression lst =
        //     let rec loop acc = function
        //         | [] -> (List.rev acc, [])
        //         | (Add|Sub)::tail -> (List.rev acc, tail)
        //         | head::tail -> loop (head::acc) tail
        //     loop [] lst
        // let testExpr =  [Var "x"; Pow; Num (Int 4); Sub; Num (Int 2); Mul; Var "x"; Pow; Num (Int 2); Add; Num (Int 7); Mul; Var "x"]
        // printfn $"getExpression - {getExpression testExpr}"

        // let rec fullTerms tList =
        //     let nextExp, rest = getExpression tList
        //     printfn $"%A{nextExp} - %A{rest}"
        //     match nextExp with
        //     | [] -> []
        //     | Var x :: _ -> Num (Int 1) :: Mul :: Var x :: Pow :: Num (Int 1) :: fullTerms rest
        //     | Num n :: Mul :: Var x :: Pow :: Num p :: _ -> nextExp @ fullTerms rest
        //     | Mul :: Var x :: _ -> Num (Int 1) :: Mul :: Var x :: Pow :: Num (Int 1) :: fullTerms rest
        //     | Num n :: Mul :: Var x :: _ -> Num n :: Mul :: Var x :: Pow :: Num (Int 1) :: fullTerms rest
        //
        // let testExpr = lexer "3x^2 + x^2 + x"
        // printfn $"%A{fullTerms testExpr}"
        let rec extractExprFrBrackets tList depth =
            match tList with
            | [] -> ([], [])
            | Rbr :: tail when depth = 1 -> ([], tail)
            | Rbr :: tail -> let inner, remaining = extractExprFrBrackets tail (depth - 1)
                             (Rbr :: inner, remaining)
            | Lbr :: tail -> let inner, remaining = extractExprFrBrackets tail (depth + 1)
                             (Lbr :: inner, remaining)
            | head :: tail -> let inner, remaining = extractExprFrBrackets tail depth
                              (head :: inner, remaining)
        // let rec getCurrentLevel tList depth =
        //     match tList with
        //     | [] -> []
        //     | Rbr :: tail ->  Rbr :: getCurrentLevel tail (depth - 1)
        //     | Lbr :: tail -> Lbr :: getCurrentLevel tail (depth + 1)
        //     | head :: tail when depth = 1 -> head :: (getCurrentLevel tail 1)
        //     | _ :: tail -> (getCurrentLevel tail depth)
        // let expr = lexer "cos(2x)"
        // printfn $"Depth 1 - %A{getCurrentLevel expr 1}"
        // let expr = lexer "sin(x) / x"
        // printfn $"Depth 1 - %A{getCurrentLevel expr 1}"
        // let expr = lexer "sin(x) / x"
        // printfn $"Depth 1 - %A{getCurrentLevel expr 1}"
        let splitByElement lst =
            let rec split acc current = function
                | [] -> List.rev (List.rev current :: acc)
                | (Add|Sub)::xs -> split (List.rev current :: acc) [] xs
                | (Lbr|Rbr)::xs -> split acc current xs
                | x::xs -> split acc (x::current) xs

            split [] [] lst
        let rec distributeTerms first second =
            match first with
            | [] -> []
            | term::rest -> (term @ [Mul; Lbr] @ second @ [Rbr]) @ [Add] @ distributeTerms rest second
        let rec distributeTermsFull (first: terminal list list) (second: terminal list list) =
            match first with
            | [] -> []
            | term::rest -> distributeTerms second term @ [Add] @ distributeTermsFull rest second


        // let first, second = lexer "2x", lexer "3" // 3x^2 + 6x + 5x + 10 = 3x^2 + 11x + 10
        // let first = splitByElement first
        // let expansion = distributeTerms first second
        // printfn $"%A{terminalListToString expansion}"
        // let expansion = distributeTermsFull first (splitByElement second)
        // printfn $"%A{terminalListToString expansion}"
        // printfn $"%A{terminalListToString (simplify expansion)}"
        // printfn $"%A{terminalListToString expansion}"
        // "x * (3 * x) + 2 * (3 * x) +  + x * (5) + 2 * (5) +  + "
        let rec getCurrentLevel tList depth =
            match tList with
            | [] -> []
            | Rbr :: tail ->  Rbr :: getCurrentLevel tail (depth - 1)
            | Lbr :: tail -> Lbr :: getCurrentLevel tail (depth + 1)
            | head :: tail when depth = 1 -> head :: (getCurrentLevel tail 1)
            | _ :: tail -> (getCurrentLevel tail depth)
        let rec getCurrentLevel tList depth =
            match tList with
            | [] -> []
            | Rbr :: tail ->  Rbr :: getCurrentLevel tail (depth - 1)
            | Lbr :: tail -> Lbr :: getCurrentLevel tail (depth + 1)
            | head :: tail when depth = 1 -> head :: (getCurrentLevel tail 1)
            | _ :: tail -> (getCurrentLevel tail depth)
        let getExpression lst =
            let rec loop acc tList (depth: int) =
                match tList with
                | [] -> (List.rev acc, [])
                | (Add|Sub as o)::tail when depth=1 -> (List.rev acc, o::tail)
                | Rbr :: tail ->  loop (Rbr::acc) tail (depth-1)
                | Lbr :: tail -> loop (Lbr::acc) tail (depth+1)
                | head::tail -> loop (head::acc) tail depth
            loop [] lst 1
        let getLastExpression lst =
            // printfn $"getLastExpression: %A{lst}"
            let rec loop acc tList =
                // printfn $"calling getExpression on: %A{tList}"
                let expr, rest = getExpression tList
                // printfn $"Expr: %A{expr}, rest: %A{rest}"
                match rest with
                | [] -> expr, acc
                | o::rest -> loop (acc@expr@[o]) rest
            loop [] lst
        let rec splitAtOperator op tList depth =
            match tList with
            | [] -> ([], [])
            | head :: tail when head = op && depth = 0 -> ([], tail)
            | Lbr :: tail ->
                let left, right = splitAtOperator op tail (depth + 1)
                (Lbr :: left, right)
            | Rbr :: tail ->
                let left, right = splitAtOperator op tail (depth - 1)
                (Rbr :: left, right)
            | head :: tail ->
                let left, right = splitAtOperator op tail depth
                (head :: left, right)
        // let expr = lexer "(3x+5)*(x+2)+5"
        // let simplified = simplify expr
        // printfn $"%A{terminalListToString simplified}"
        //
        // let terminals = lexer "2 + 4 + (3 * x + 5) * (x + 2) + 5 - 3"
        // printfn $"Doing product rule on %A{terminalListToString terminals}"
        // let leftExpr, rightExpr = splitAtOperator Mul terminals 0
        // printfn $"leftExpr: %A{terminalListToString leftExpr}, rightExpr: %A{terminalListToString rightExpr}"
        // // let rightExpr, rest = extractExprFrBrackets rightExpr 1
        // // printfn $"rightExpr: %A{terminalListToString rightExpr}, rest: %A{terminalListToString rest}"
        // let rightExpr, rest = getExpression rightExpr
        // printfn $"rightExpr: %A{terminalListToString rightExpr}, rest: %A{terminalListToString rest}"
        // let leftExpr, rest = getLastExpression leftExpr
        // printfn $"leftExpr: %A{terminalListToString leftExpr}, beginning %A{terminalListToString rest}"

        // let first = splitByElement leftExpr
        // printfn $"first: %A{first}"
        // let expansion = distributeTermsFull first (splitByElement rightExpr)
        // printfn $"%A{terminalListToString expansion}"
        // printfn $"%A{terminalListToString (simplify expansion)}"


        // let innerExpr, remaining = extractExprFrBrackets expr 1
        // printfn $"%A{terminalListToString innerExpr} - %A{terminalListToString remaining}"

        // let expansion = distribute first second
        // printfn $"%A{terminalListToString expansion}"
        // let simp = simplify
        // printfn $"Expanding - %A{(terminalListToString (simplify expr))}"

        // printfn "\nDifferentiate (3x+5)/(x^2-4)"
        // let toDiff = lexer "(3x+5)/(x^2-4)"
        // // (3x+5)^3 + cos(2x) -> 9(3x+5)^2 - 2sin(2x)
        // printTokenList toDiff
        // let differentiated = differentiate toDiff
        // printTokenList differentiated
        // printfn $"{(terminalListToString differentiated)}"
        // let simplified = simplify differentiated
        // printTokenList simplified
        // printfn $"{(terminalListToString simplified)}"
        // // printfn "((3x^2 + -12) + (-6x^2 + -10x)) / ((x^2 + -4)^2)"
        // printfn "(3x^2 + -12 + -6x^2 + -10x) / ((x^2 + -4)^2)"
        // printfn $"3 * (x^2 - 4) - (3 * x + 5) * 2 * x - 0 / (x^2 - 4)^2 - Should be (-3x^2 - 10x - 12) / (x^2 - 4)^2"


        // let expr = lexer "(3x^2 + -12 + -6x^2 + -10x) / ((x^2 + -4)^2)"
        // let expr = lexer "3x^2 + -12 + -6x^2 + -10x"
        // printfn $"gE: %A{getExpression expr}"
        // printfn $"eefb: %A{extractExprFrBrackets expr 1}"
        // printfn $"sao: %A{splitAtOperator Add expr 0}"
        // let a = [[1; 2; 3]; [4; 5; 6]]
        // let b = [[Add; Sub; Add], [Mul; Div; Add]]
        // printfn $"{b}, {b.Length}"
        // let c = splitAtOperator Add expr 0
        // printfn $"%A{c}, {[c].Length}"

        // let leftExpr = lexer "(3x+5)^2"
        // let cond = match (List.rev leftExpr) with | Num _::Pow::_ -> false | _ -> true
        // printfn $"{cond}"
        // let leftExpr = lexer "(3x+5)"
        // let cond = match (List.rev leftExpr) with | Num _::Pow::_ -> false | _ -> true
        // printfn $"{cond}"
        // let leftExpr = lexer "(3x+5)^2"
        // let cond =
        // printfn $"{cond}"
        // let leftExpr = lexer "(3x+5)"
        // let cond = match (List.rev leftExpr) with | Num _::Pow::_ -> false | _ -> true
        // printfn $"{cond}"



        // let rec convert tList =
        //     match tList with
        //     | Num n :: Mul :: Var v :: Pow :: Num p :: tail -> Num n :: Mul :: Var v :: Pow :: Num p :: convert tail
        //     | Var v :: Pow :: Num p :: tail -> (Num (Int 1)) :: Mul :: Var v :: Pow :: Num p :: convert tail
        //     | Num n :: Mul :: Var v :: tail -> Num n :: Mul :: Var v :: Pow :: (Num (Int 1)) :: convert tail
        //     | Num n :: tail -> Num n :: Mul :: Var "x" :: Pow :: (Num (Int 0)) :: convert tail
        //     | a::tail -> a :: convert tail
        //     | [] -> []
        //
        //
        // let rec mergeTerms tList =
        //     let mutable termMap = Map.empty<terminal list, Value>
        //     let rec loop tList =
        //         match tList with
        //         | Num n :: Mul :: Var v :: Pow :: Num p :: tail ->
        //             let term = [Var v; Pow; Num p]
        //             termMap <- Map.add term (add n (if termMap.ContainsKey term then termMap[term] else (Int 0))) termMap
        //             loop tail
        //         | _::tail -> loop tail
        //         | [] -> ()
        //     loop tList
        //     termMap
        //
        // let rebuildExpression (termMap: Map<terminal list, Value>) : terminal list =
        //     termMap
        //     |> Map.toList
        //     |> List.sortByDescending (fun (term, _) -> match term with | Var _ :: Pow :: Num p :: _ -> toFloat p) // sort by power
        //     |> List.fold (fun acc (term, coeff) -> acc @ [Num coeff; Mul] @ term @ [Add]) []

        let combineLikeTerms tList =
            // let rec convert tList =
            //     match tList with
            //     | Num n :: Mul :: Var v :: Pow :: Num p :: tail -> Num n :: Mul :: Var v :: Pow :: Num p :: convert tail
            //     | Var v :: Pow :: Num p :: tail -> (Num (Int 1)) :: Mul :: Var v :: Pow :: Num p :: convert tail
            //     | Num n :: Mul :: Var v :: tail -> Num n :: Mul :: Var v :: Pow :: (Num (Int 1)) :: convert tail
            //     | Num n :: tail -> Num n :: Mul :: Var "x" :: Pow :: (Num (Int 0)) :: convert tail
            //     | a::tail -> a :: convert tail
            //     | [] -> []
            let rec convert acc tail =
                match tail with
                | Num n :: Mul :: Var v :: Pow :: Num p :: tail ->  convert (acc@[Num n; Mul; Var v; Pow; Num p]) tail
                | Var v :: Pow :: Num p :: tail -> convert (acc@[Num (Int 1); Mul; Var v; Pow; Num p]) tail
                | Num n :: Mul :: Var v :: tail -> convert (acc@[Num n; Mul; Var v; Pow; Num (Int 1)]) tail
                | Num n :: tail -> convert (acc@[Num n; Mul; Var "x"; Pow; Num (Int 0)]) tail
                | (Add|Lbr)::tail -> convert (acc@[Add]) tail
                | _ -> (acc, tail)

            let converted, rest = convert [] tList
            printfn $"Converted: %A{unlexer converted}, The rest: {unlexer rest}"
            let mutable termMap = Map.empty<terminal list, Value>
            let rec loop tList =
                match tList with
                | Num n :: Mul :: Var v :: Pow :: Num p :: tail ->
                    let term = [Var v; Pow; Num p]
                    termMap <- Map.add term (add n (if termMap.ContainsKey term then termMap[term] else (Int 0))) termMap
                    loop tail
                | _::tail -> loop tail
                | [] -> ()
            loop converted
            printfn $"termmap: {termMap}"
            (termMap
            |> Map.toList
            |> List.sortByDescending (fun (term, _) -> match term with | Var _ :: Pow :: Num p :: _ -> toFloat p) // sort by power
            |> List.fold (fun acc (term, coeff) -> acc @ [Num coeff; Mul] @ term @ [Add]) []
            , rest)

        // let expr = lexer "(3x^2 + -12 + -6x^2 + -10x) / ((x^2-4)^2)"
        // let combined, rest = combineLikeTerms expr
        // printfn $"%A{combined}"
        // printfn $"%A{terminalListToString combined}"
        // printfn $"%A{terminalListToString (simplify combined)}"


        //     let sortedTerms =
        //         varMap
        //         |> Map.toList
        //         |> List.sortByDescending (fun (term, _) ->
        //             match term with
        //             | Var _ :: Pow :: Num (Int p) :: _ -> p  // Sort by power if x^p
        //             | Var _ :: _ -> 1                        // x^1 if no explicit power
        //             | Num _ :: _ -> 0                        // Constants last
        //             | _ -> 0)
        //
        //     sortedTerms
        //     |> List.fold (fun acc (term, coeff) ->
        //         match term, coeff with
        //         | _, 0 -> acc  // Skip terms with a 0 coefficient
        //         | [Var v], 1 -> acc @ [Var v]  // For cases like 1 * x
        //         | [Var v], -1 -> acc @ [Sub; Var v]  // For cases like -1 * x
        //         | [Var v], _ -> acc @ [Num (Int coeff); Mul; Var v]  // Coeff * x
        //         | [Var v; Pow; Num (Int p)], 1 -> acc @ [Var v; Pow; Num (Int p)]  // x^p
        //         | [Var v; Pow; Num (Int p)], -1 -> acc @ [Sub; Var v; Pow; Num (Int p)]  // -x^p
        //         | [Var v; Pow; Num (Int p)], _ -> acc @ [Num (Int coeff); Mul; Var v; Pow; Num (Int p)]  // Coeff * x^p
        //         | [Num n], _ -> acc @ [Num (Int (coeff * n))]  // Constant terms
        //
        //         | _ -> acc  // Add other terms as they are if they don't match above patterns
        //     )

            // let final =  Map.iter (fun key value -> (value::Mul::key) varMap)
            // final

            // varMap
            // |> List.fold (fun acc (term, coeff) ->
            //     match term, coeff with
            //     | _, 0 -> acc  // Skip terms with a 0 coefficient
            //
            //     | [Var v], 1 -> acc @ [Var v]  // For cases like 1 * x
            //     | [Var v], -1 -> acc @ [Sub; Var v]  // For cases like -1 * x
            //     | [Var v], _ -> acc @ [Num (Int coeff); Mul; Var v]  // Coeff * x
            //
            //     | [Var v; Pow; Num (Int p)], 1 -> acc @ [Var v; Pow; Num (Int p)]  // x^p
            //     | [Var v; Pow; Num (Int p)], -1 -> acc @ [Sub; Var v; Pow; Num (Int p)]  // -x^p
            //     | [Var v; Pow; Num (Int p)], _ -> acc @ [Num (Int coeff); Mul; Var v; Pow; Num (Int p)]  // Coeff * x^p
            //
            //     | [Num n], _ -> acc @ [Num (Int (coeff * n))]  // Constant terms
            //
            //     | _ -> acc  // Add other terms as they are if they don't match above patterns
            // ) []



        "((3x + 5) + x + 3) * 2"
        // let expr = lexer "((3x+5)+(x;+3))^2 + ( (4x+3)/2 + 5)"
        // let expr = lexer "((3x+5)+(x+3)) / ((2x+7)+(x+4))"
        // let expr = lexer "(3x^2 + -12 + -6x^2 + -10x) / ((x^2 + -4)^2)"
        // let simp = simplify expr
        // printfn $"{terminalListToString simp}"
        // printfn "Done"

        // let expr = lexer "(3 * x + 5) * 2"
        // printfn $"{extractExprFrBrackets expr 1}"

        try

            let tokenList = lexer(getInputString())
            printTokenList tokenList |> ignore
            // parser tokenList |> ignore
            // let _, result = parseAndEval tokenList
            let result = differentiate tokenList
            printfn $"{(unlexer result)}"
            printfn $"Result = {unlexer (simplify result)}"
        with
            | LexerError(c) -> printfn $"Lexer Error, invalid token {c}"
            | ParserError -> printfn "Error parsing"
            | VarUndefined(v) -> printfn $"Variable {v} is not defined"
            | :? OverflowException -> printfn $"Overflow error, exceeded max value for int32"
        interpret ()
    interpret()
