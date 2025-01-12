module InterpreterModule.Differentiator
open InterpreterModule.Types


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

/// extract sub-expression contained within opened brackets e.g. 3+3(x+3)^2) * 3 -> 3+3(x+3)^2
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

let getNextTerm tList =  // 2*(3x+5)^2 + 8x^3 -> 2*(3x+5)^2
    let rec loop acc tList depth =
        match tList with
        | [] -> (List.rev acc, [])
        | Add|Sub as o::tail when depth=1 -> (List.rev acc, o::tail)
        | Rbr :: tail ->  loop (Rbr::acc) tail (depth-1)
        | Lbr :: tail -> loop (Lbr::acc) tail (depth+1)
        | head::tail -> loop (head::acc) tail depth
    loop [] tList 1
let getLastTerm lst =
    let rec loop acc tList =
        let expr, rest = getNextTerm tList
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

let rec simplify terminals =
    let combineLikeTerms tList =
        let rec convert acc tail = // expands each term to form ax^n
            match tail with
            | Num n :: Mul :: Var v :: Pow :: Num p :: tail when (tail.Length=0 || tail.Head=Add) ->  convert (acc@[Num n; Mul; Var v; Pow; Num p]) tail
            | Var v :: Pow :: Num p :: tail when (tail.Length=0 || tail.Head=Add) -> convert (acc@[Num (Int 1); Mul; Var v; Pow; Num p]) tail
            | Num n :: Mul :: Var v :: tail when (tail.Length=0 || tail.Head=Add) -> convert (acc@[Num n; Mul; Var v; Pow; Num (Int 1)]) tail
            | Num n :: tail when (tail.Length=0 || tail.Head=Add) -> convert (acc@[Num n; Mul; Var "x"; Pow; Num (Int 0)]) tail
            | Add as o::tail -> convert (acc@[o]) tail
            | _ -> (acc, tail)

        let converted, rest = convert [] tList
        printfn $"Converted: %A{unlexer converted}, The rest: {unlexer rest}"
        let mutable termMap = Map.empty<terminal list, Value>
        let rec loop tList =  // counts number of each coefficient and stores in a map
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
        |> List.fold (fun acc (term, coeff) -> acc @ simplify (Num coeff :: Mul ::term) @ [Add] ) [] // rebuild expression from map
        , rest)


    let endOfTerm = function // checks if tList starts with a * or ^ or /
        | (Mul|Div|Pow) :: _ -> false
        | _ -> true

    let splitTerms lst = // 3x + 5 + x^2 -> [3x], [5], [x^2]
        let rec split acc current depth = function
            | [] -> List.rev (List.rev current :: acc)
            | (Add|Sub)::tail when current.Length>0 && depth=1 -> split (List.rev current :: acc) [] 1 tail
            | Rbr :: tail -> split acc (Rbr::current) (depth-1) tail
            | Lbr :: tail -> split acc (Lbr::current) (depth+1) tail
            | x::tail -> split acc (x::current) depth tail

        split [] [] 1 lst
    ///  expands out (a * b * ... ) * c
    let rec expandBracket first second = // (a * b * ... ) * c
            printfn $"Expanding %A{first} * %A{second}"
            match first with
            | [] -> []
            | term::rest -> (term @ [Mul; Lbr] @ second @ [Rbr]) @ (if rest.IsEmpty then [] else [Add]) @ expandBracket rest second
    ///  expands out (a * b * ...) * (c * d * ...)
    let rec expandBrackets first second =
        printfn $"Full Expanding %A{first} * %A{second}"
        match first with
        | [] -> []
        | term::rest -> expandBracket second term @ (if rest.IsEmpty then [] else [Add]) @ expandBrackets rest second
    /// checks the next term is in the form ax^n, x^n or a
    let isPureTerm = function
        | Num _ :: t | Num _ :: Mul :: Var _ :: t | Num _ :: Mul :: Var _ :: Pow :: Num _ :: t when startsWithAny [Add; Rbr] t || t.IsEmpty -> true
        | _ -> false

    let rec simplifyExpr terminals acc =
        // printfn $"simplifyExpr - %A{terminals} - %A{acc}  | %A{unlexer terminals} - %A{unlexer acc}"

        match terminals with
        | [] -> acc // finished
        | Sub :: Num n :: rest -> simplifyExpr (Add::Num (sub (Int 0) n)::rest) acc // Sub; Num n -> Add; Num -n
        | Sub::Lbr::rest -> simplifyExpr (acc@[Add; Num(Int -1); Mul; Lbr]@rest) []
        | [ (Add|Sub) ] -> simplifyExpr [] acc
        | Add|Sub as o1 :: (Add|Sub as o2) :: rest  when o1=o2 -> simplifyExpr (Add::rest) acc // ++ -> +  -- -> +
        | Add|Sub as o1 :: (Add|Sub as o2) :: rest  when o1<>o2 -> simplifyExpr (Sub::rest) acc // +- -> -
        | Num (Int 0) :: Add :: rest -> simplifyExpr rest acc // 0 +
        | (Add | Sub) :: Num (Int 0) :: rest when endOfTerm rest -> simplifyExpr rest acc // - 0, + 0 when at the end of a term
        | Num a :: Add :: Num b :: rest when endOfTerm rest && endOfTerm (List.rev acc) -> simplifyExpr (acc@[Num(add a b)]@rest) [] // add two independent numbers
        | a :: Add :: b :: rest when a=b && endOfTerm rest && endOfTerm (List.rev acc) -> simplifyExpr (acc@[Lbr; Num (Int 2); Mul; a; Rbr]@rest) [] // x+x = 2x
        | Num n :: Mul :: a :: Add :: b :: rest when a=b && endOfTerm rest -> simplifyExpr (acc@[Lbr; Num (add n (Int 1)); Mul; a; Rbr]@rest) [] // ax+x = (a+1)x

        | Num (Int 1) :: Mul :: rest | Mul :: Num (Int 1) :: rest-> simplifyExpr rest acc // 1*x or x*1 -> x

        | Num a :: Mul :: Num b :: rest -> simplifyExpr (acc@[Num(mul a b)]@rest) [] // n1*n2
        | Var v1 :: Mul :: Var v2 :: Pow :: Num n :: rest when v1=v2 -> simplifyExpr (acc@[Var v1; Pow; Num (add n (Int 1))]@rest) [] // x*x^n -> x^(n+1)
        | Var v1 :: Pow :: Num a :: Mul :: Var v2 :: Pow :: Num b :: rest when v1=v2 -> simplifyExpr (acc@[Var v1; Pow; Num (add a b)]@rest) [] // x^a * x^b -> x^(a*b)
        | a :: Mul :: b :: rest when a=b -> simplifyExpr (acc@[a; Pow; Num (Int 2)]@rest) [] // x*x -> x^2
        | Pow :: Num a :: Pow :: Num b :: rest -> simplifyExpr (acc@[Pow; Num (mul a b)]@rest) [] // x^a^b = x^(a+b)
        | Pow :: Num (Int 1) :: rest -> simplifyExpr (acc@rest) [] // x^1 -> x
        | Var _ :: Pow :: Num (Int 0) :: rest -> simplifyExpr (acc@[Num (Int 1)]@rest) [] // x^0 -> 1

        | Num a :: Pow :: Num b :: rest -> simplifyExpr (acc@[Num (pow a b)]@rest) [] // a^b

        | Num a :: Mul :: Var v :: Mul :: Num b :: rest -> simplifyExpr (acc @ [Num (mul a b); Mul; Var v]@rest) [] // a*x*b = (a*b)*x
        | a :: Mul :: b :: Mul :: c :: rest when a=c -> simplifyExpr (acc @ [b; Mul; a; Mul; c]@rest) [] // x * 2 * x -> 2 * x * x


        | Num (Int 0) :: (Mul|Div) :: Lbr :: rest -> let a, remaining = getExprFromBrackets rest 1 // 0*()
                                                     // printfn $"Doing 0x() - $%A{unlexer a} | $%A{unlexer remaining}"
                                                     simplifyExpr (acc@(Num (Int 0)::remaining)) []
        | Num (Int 0) :: (Mul|Div) :: rest -> let toRemove, remaining = getNextTerm rest
                                              // printfn $"Doing 0x() 2 - $%A{unlexer toRemove} | $%A{unlexer remaining}"
                                              simplifyExpr (acc@(Num (Int 0)::remaining)) []
        | Mul :: Num (Int 0) :: rest ->
            let leftPart, before = getLastTerm acc
            let rightPart, after = getNextTerm rest
            // printfn $"Removing *0, before: {unlexer before}, leftPart {unlexer leftPart}, rightPart {unlexer rightPart}, after {unlexer after}"
            simplifyExpr (before@after) []
        | Sin|Cos|Tan|Log|Exp as func :: Lbr :: rest ->
            // printfn "Passing over function brackets"
            let inner, remaining = getExprFromBrackets rest 1
            let simplifiedInner = simplify inner  // Keep brackets around function arguments
            simplifyExpr remaining (acc@[func;Lbr]@simplifiedInner@[Rbr])

        | Lbr :: rest ->
            // printfn $"Processing %A{unlexer ([Lbr]@rest)}"
            let inner, remaining = getExprFromBrackets rest 1
            // printfn $"Inner %A{unlexer inner}, rest %A{unlexer remaining}"
            let simplifiedInner = simplify inner
            let cond = (containsAny [Add; Sub] simplifiedInner) && (rest.Head <> Lbr || (startsWithAny [Div; Mul; Pow] remaining))
            let cond1 = (List.contains Add simplifiedInner || List.contains Sub simplifiedInner) && (rest.Head <> Lbr || (remaining.Length > 0 && (remaining.Head=Div || remaining.Head=Mul || remaining.Head=Pow)))
            // printfn $"Simplified %A{unlexer inner} -> %A{unlexer simplifiedInner} - %A{unlexer remaining} - {cond} {cond1} - {rest.Head} - %A{acc} - original: %A{unlexer ([Lbr]@rest)}"
            match simplifiedInner with
            | _ when ((acc.Length = 0 || (startsWithAny [Add; Sub] (List.rev acc))) && (not (startsWithAny [Div; Mul; Pow] remaining))) -> simplifyExpr (acc @ simplifiedInner @ remaining) []

            | _ when (containsAny [Add; Sub; Div] simplifiedInner) && (rest.Head <> Lbr || (startsWithAny [Div; Mul] remaining) || (startsWithAny [Div; Mul] (List.rev acc))) -> simplifyExpr remaining (acc@[Lbr]@simplifiedInner@[Rbr])
            | _ -> simplifyExpr (acc @ simplifiedInner @ remaining) []


        | Mul :: b :: rest when ((List.last acc)=Rbr || b=Lbr) -> // expand out bracket * bracket
            // printfn $"Doing Mul rule on %A{unlexer terminals}, before %A{unlexer acc}"
            let (leftExprU, before), (rightExprU, after) = (getLastTerm acc), (getNextTerm (b::rest))
            // printfn $"before: %A{unlexer before}, leftExpr: %A{unlexer leftExprU}, rightExpr: %A{unlexer rightExprU}, after: %A{unlexer after}"
            let leftExpr = simplify leftExprU
            // printfn $"Simplified leftExpr: %A{leftExpr}"
            let rightExpr = simplify rightExprU
            // printfn $"Simplified rightExpr: %A{rightExpr}"
            // printfn $"final before: %A{unlexer before}, leftExpr: %A{unlexer leftExpr}, rightExpr: %A{unlexer rightExpr}, after: %A{unlexer after}"
            match (leftExpr, rightExpr) with
            // one of the brackets contains multiple terms so needs expanding, and neither brackets are raised to a power
            | _ when (containsAny [Add; Sub] (getCurrentLevel leftExpr 1) || containsAny [Add; Sub] (getCurrentLevel rightExpr 1)) && (match (List.rev leftExpr) with | Num _::Pow::Rbr::_ -> false | _ -> true && match (List.rev rightExpr) with | Num _::Pow::Rbr::_ -> false | _ -> true) && not (containsAny [Div] (leftExpr@rightExpr)) ->
                let expansion = expandBrackets (splitTerms leftExpr) (splitTerms rightExpr)
                // printfn $"expanded to %A{unlexer expansion}"
                // printfn $"Result: %A{unlexer (before@expansion@after)}"
                simplifyExpr (before@Lbr::expansion@Rbr::after) []
            | _, Lbr::Num n::[ Rbr ] | _, [Num n] -> simplifyExpr (before@[Num n]@[Mul]@leftExpr@after) [] // something like 3x^2 * 3, swap round so numbers can be combined
            | Num a :: Mul :: restL, Num b :: Mul :: restR -> simplifyExpr (before@Num (mul a b) :: Mul::restL@Mul::restR@after) []
            | _ when containsAny [Div] (getCurrentLevel rightExpr 1) -> simplifyExpr after before@Lbr::leftExpr@Rbr::Mul::Lbr::rightExpr@[Rbr]
            | _ when leftExpr<>leftExprU || rightExpr<>rightExprU-> simplifyExpr (before@leftExpr@Mul::rightExpr@after) []
            | _ -> simplifyExpr (b::rest) (acc @ [Mul])

        | a :: Div :: b :: rest when (fst (getNextTerm (b::rest)))=(fst (getLastTerm (acc@[a]))) -> // (3x+5)/(3x+5) -> 1
            // printfn "Cancelling fraction"
            let _, start = getLastTerm (acc@[a])
            let _, rest = getNextTerm (b::rest)
            simplifyExpr (start@[Num (Int 1)]@rest) []
        | Pow :: Lbr :: rest ->

            let expExpression, rest = getExprFromBrackets rest 1
            // printfn $"Skipping power expression: acc: {unlexer acc}, expExpr: {unlexer expExpression}, rest: {unlexer rest}"
            simplifyExpr rest (acc@Pow::Lbr::expExpression@[Rbr]) // keeps operators attached to brackets
        | Pow|Mul|Div as o :: exp :: rest when acc.Head=Rbr-> simplifyExpr rest (acc@[o; exp]) // keeps operators attached to brackets


        // | (Num a :: Mul :: Var v :: Pow :: Num p :: Add :: Num p1 :: rest) as full when isPureTerm (Num p1::rest) ->
        | Add :: b :: rest when isPureTerm ((fst (getLastTerm acc))@[Add]) && isPureTerm (b::rest) ->  // combine terms like 3x^2 + 6x^2
            let lastTerm, before = getLastTerm acc
            let full = lastTerm@Add::b::rest
            // printfn $"Combining like terms on {unlexer full}"
            let combinedTerms, rest = combineLikeTerms full
            let combinedTerms = if rest.IsEmpty then combinedTerms[0..combinedTerms.Length-2] else combinedTerms // strip trailing +
            // printfn $"Combined like terms - {unlexer combinedTerms}, rest: {unlexer rest}"
            simplifyExpr rest (before@combinedTerms)



        | head :: tail -> simplifyExpr tail (acc @ [head])



    simplifyExpr terminals []


let differentiate tList tv = // tv target variable to differentiate with respect to
    printfn $"Differentiating %A{unlexer tList}"
    let tList = simplify tList
    printfn $"Simplified to %A{unlexer tList}"

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

        | _ when List.contains Div (getCurrentLevel tList 1) -> // Quotient Rule - f(x) / g(x) -> (f'(x) * g(x) - f(x) * g'(x)) / g(x)^2
            // printfn $"Doing quotient rule on %A{unlexer tList}"
            let leftExpr, rightExpr = splitAtOperator Div tList 0
            // printfn $"leftExpr: %A{unlexer leftExpr}, rightExpr: %A{unlexer rightExpr}"
            let rightExpr, rest = getNextTerm rightExpr
            // printfn $"rightExpr: %A{rightExpr}, rest: %A{rest}"
            let leftExpr, start = getLastTerm leftExpr
            let leftDiff, rightDiff = (diff leftExpr, diff rightExpr)
            // printfn $"diff leftExpr: %A{unlexer leftDiff}, diff rightExpr: %A{unlexer rightDiff}"
            let num = [Lbr; Lbr] @ leftDiff @ [Rbr; Mul; Lbr] @ rightExpr @ [Rbr; Rbr; Sub; Lbr; Lbr] @ leftExpr @ [Rbr; Mul; Lbr] @ rightDiff @ [Rbr; Rbr]
            let denom = [Lbr] @ rightExpr @ [Pow; Num (Int 2); Rbr]
            // printfn $"Done Quotient rule - %A{num} / %A{denom} - %A{unlexer num} / %A{unlexer denom}"
            start @ [Lbr] @ num @ [Rbr; Div] @ denom @ diff rest
        | _ when List.contains Mul (getCurrentLevel tList 1) && doProductRule tList -> // Product Rule - f(x) * g(x) -> f'(x) * g(x) + f(x) * g'(x)
            // printfn $"Doing product rule on %A{unlexer tList}"
            let leftExpr, rightExpr = splitAtOperator Mul tList 0
            // printfn $"leftExpr: %A{leftExpr}, rightExpr: %A{rightExpr}"
            let rightExpr, rest = getNextTerm rightExpr
            // printfn $"rightExpr: %A{rightExpr}, rest: %A{rest}"
            let leftExpr, start = getLastTerm leftExpr
            // printfn $"leftExpr: %A{leftExpr}, start: %A{start}"
            let leftDiff, rightDiff = (diff leftExpr, diff rightExpr)
            // printfn $"diff leftExpr: %A{leftDiff}, diff rightExpr: %A{rightDiff}"
            // let res = [Lbr] @ leftDiff @ [Rbr; Mul; Lbr] @ rightExpr @ [Rbr; Add; Lbr] @ leftExpr @ [Rbr; Mul; Lbr] @ rightDiff @ [Rbr]
            // printfn $"pr result: %A{ res} <- %A{leftExpr}*%A{rightExpr}  %A{unlexer res}"
            start @ [Lbr; Lbr] @ leftDiff @ [Rbr; Mul; Lbr] @ rightExpr @ [Rbr; Add; Lbr] @ leftExpr @ [Rbr; Mul; Lbr] @ rightDiff @ [Rbr; Rbr] @ diff rest

        | _ when List.contains Pow (getCurrentLevel tList 1) -> // Chain Rule - (f)^n -> (f)^(n-1) * f'
            // printfn $"Doing chain rule on %A{unlexer tList}"
            let leftExpr, rightExpr = splitAtOperator Pow tList 0
            // printfn $"leftExpr: %A{unlexer leftExpr}, rightExpr: %A{unlexer rightExpr}"
            let rightExpr, rest = getNextTerm rightExpr
            // printfn $"rightExpr: %A{rightExpr}, rest: %A{rest}"
            let leftExpr, start = getLastTerm leftExpr
            // printfn $"leftExpr: %A{leftExpr}, start: %A{start}"
            // let innerExpr, remaining = getExprFromBrackets tail 1
            // printfn $"innerExpr: %A{innerExpr}, remaining: %A{remaining}"
            // let p = match rightExpr with | [Num p] -> p | _ -> raise ParserError // only simple powers for now
            // let baseExpr = match leftExpr with | Lbr :: tail -> fst (getExprFromBrackets tail 1) | _ -> leftExpr
            // let expExpression = match rightExpr with | Lbr :: tail -> fst (getExprFromBrackets tail 1) | _ -> rightExpr
            let start, baseExpr = match (List.rev leftExpr) with
                                  | Rbr::tail -> let baseExpr, tail = getExprFromBrackets tail 1
                                                 // printfn $"baseExr: %A{unlexer baseExpr}, tail: %A{unlexer tail}"
                                                 start @ (List.rev tail), (List.rev baseExpr)@[Rbr]

                                  | a::tail -> start @ (List.rev tail), [a]
            let rest, expExpression = match rightExpr with
                                       | Lbr::tail -> let baseExpr, tail = getExprFromBrackets tail 1
                                                      rest @ tail, baseExpr

                                       | a::tail -> rest @ tail, [a]

            // let expExpression = rightExpr
            // printfn $"start: %A{unlexer start}, baseExpr: %A{unlexer baseExpr}, expExpression: %A{unlexer expExpression}, end: %A{unlexer rest}"
            match expExpression with  // the thing in the power
            | [Num p] -> let newPow = sub p (Int 1)
                         // printfn "Doing simple power chain rule"
                         let innerDiff = diff baseExpr
                         // printfn $"diff innerExpr: %A{innerDiff}"
                         // printfn $"cr result: %A{Lbr::innerDiff@[Rbr; Mul] @ [Num p; Mul] @ Lbr :: baseExpr @ [Rbr; Pow; Num newPow]}"
                         start @ Lbr::innerDiff@[Rbr; Mul] @ [Num p; Mul] @ baseExpr @ [Pow; Num newPow] @ diff rest
            | _ -> // f(x) ^ g(x) -> (f(x) ^ g(x)) * (g(x)*ln(f(x)))'
                // printfn $"Doing complicated power chain rule, base: %A{baseExpr}, exponent: %A{expExpression}"
                let toDiff = Lbr::expExpression@Rbr::Mul::Log::Lbr::baseExpr@[Rbr]
                let differentiated = diff toDiff
                let result = baseExpr@Pow::Lbr::expExpression@Rbr::Mul::differentiated
                // printfn $"finished complicated power chain rule: %A{result} - {unlexer result}"
                start @ result @ diff rest


        | Sin|Cos|Tan|Exp|Log as f :: Lbr :: tail -> // built in functions
            // printfn $"Doing {f} - %A{tail}"
            let innerExpr, remaining = getExprFromBrackets tail 1
            // printfn $"innerExpr: %A{innerExpr}, remaining: %A{remaining}"
            let innerDiff = diff innerExpr
            let funcDiff = match f with
                           | Sin -> [Mul; Cos; Lbr] @ innerExpr @ [Rbr] // x' * cos(x)
                           | Cos -> [Mul; Sub; Sin; Lbr] @ innerExpr @ [Rbr] // x' * -sin(x)
                           | Exp -> [Mul; Exp; Lbr] @ innerExpr @ [Rbr] // x' * exp(x)
                           | Tan ->  Div :: Cos :: Lbr :: innerExpr @ [Rbr; Pow; Num (Int 2)] // x' * sec^2(x) or 1/cos^2
                           | Log ->  Div :: Lbr :: innerExpr @ [Rbr] // x'/x
            // printfn $"Diff {f}: %A{funcDiff @ innerExpr @ [Rbr; Mul] @ Lbr::innerDiff@[Rbr]}"
            [Lbr] @ innerDiff@[Rbr] @ funcDiff @  diff remaining // x'
        | Var v :: Pow :: Num n :: tail when v=tv -> // Power Rule - x^n -> n * x^(n-1)
            // printfn "Doing power rule 2"
            [Num n; Mul; Var v; Pow; Num (sub n (Int 1) ) ] @ diff tail
        | Num n :: Mul :: tail -> Num n :: Mul :: diff tail
        | Num _ :: tail -> [Num (Int 0)] @ diff tail  // number ->
        | Var _ :: Mul :: Num n :: tail -> [Num n] @ diff tail
        | Var v :: tail when v<>tv -> [Num (Int 0)] @ diff tail  // number -> 0
        | Var v :: tail when v=tv -> [Num (Int 1)] @ diff tail  // x -> 1
        | Lbr :: tail ->
            let expr, rest = getExprFromBrackets tail 1
            // printfn $"Opening brackets on (${unlexer tail} -> Expr: {unlexer expr}, Rest: {unlexer rest}"
            diff (expr @ rest)

        | _ -> raise ParserError
    diff tList

let diffAndSimplify tList targetVar = simplify (differentiate tList targetVar)
let diffToString tList targetVar = unlexer (diffAndSimplify tList targetVar)
let simplifyToString eqn = unlexer (simplify eqn)




