module InterpreterModule.Tests

open Xunit
open InterpreterModule.Interpreter
open Microsoft.FSharp.Collections
open System

//helper
let inline floatEqual (a: float) (b: float) (tolerance: float) =
    abs(a - b) < tolerance

//basic arithmetic
[<Fact>]
let ``Test Basic Arithmetic Operations`` () =
    Assert.Equal(Int 5, add (Int 2) (Int 3))
    Assert.Equal(Float 5.5, add (Int 2) (Float 3.5))
    Assert.Equal(Float 2.0, div (Float 4.0) (Int 2))
    Assert.Equal(Float 0.5, div (Int 1) (Int 2))

//lexer and parsing
[<Fact>]
let ``Test Lexer and Parsing`` () =
    varMap <- Map.empty.Add("x", Int 1)
    let tokens = lexer "x^2 + 5"
    Assert.Equal<terminal list>([Var "x"; Pow; Num (Int 2); Add; Num (Int 5)], tokens)
    let result = parser tokens
    Assert.Empty(result :> seq<_>)


//test eval
[<Fact>]
let ``Test Expression Evaluation`` () =
    let expr = lexer "2 + 2"
    let _, result = parseAndEval expr
    Assert.Equal(Int 4, result)

//test plot
[<Fact>]
let ``Test Simple Function Plot`` () =
    let funcDef = "f(x) = x^2"
    let minX, maxX = "-2", "2"
    let _, varMap, funcMap = Interpreter.main(funcDef, Map.empty, Map.empty)
    let points, _, _ = plot("f(x)", minX, maxX, varMap, funcMap)
    Assert.True(points.Length > 0)
    let firstPointY = snd points.Head
    let lastPointY = snd points.[points.Length - 1]
    Assert.True(floatEqual firstPointY 4.0 0.001, $"Expected 4.0, got {firstPointY}")
    Assert.True(floatEqual lastPointY 4.0 0.001, $"Expected 4.0, got {lastPointY}")



//test integral
[<Fact>]
let ``Test Numerical Integration for Quadratic Function`` () =
    let funcDef = "f(x) = x^2"
    let integralInput = "integral(f, 0, 10, 100)"
    let _, varMap, funcMap = Interpreter.main(funcDef, Map.empty, Map.empty)
    let _, result = parseAndEval(lexer integralInput)
    match result with
    | Float value -> Assert.True(floatEqual value 333.3 0.1)
    | _ -> Assert.Fail("Result should be a float")

//test calc sine func
[<Fact>]
let ``Test Numerical Integration for Sine Function`` () =
    let funcDef = "f(x) = sin(x)"
    let integralInput = "integral(f, 0, 3.14, 100)"
    let _, varMap, funcMap = Interpreter.main(funcDef, Map.empty, Map.empty)
    let _, result = parseAndEval(lexer integralInput)
    match result with
    | Float value -> Assert.True(floatEqual value 2.0 0.1)
    | _ -> Assert.Fail("Result should be a float")

//test div by 0
[<Fact>]
let ``Test Division by Zero`` () =
    let expr = lexer "1 / 0"
    let result = parseAndEval expr
    match result with
    | (_, Float value) when Double.IsInfinity(value) -> Assert.True(true)
    | _ -> Assert.Fail("Expected division by zero behavior")


//test var handling
[<Fact>]
let ``Test Variable Assignment and Retrieval`` () =
    let expr = lexer "a = 5"
    let _, result = parseAndEval expr
    Assert.Equal(Int 5, Map.find "a" varMap)
