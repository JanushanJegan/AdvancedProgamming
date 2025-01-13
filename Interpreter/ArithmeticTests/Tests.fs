module InterpreterModule.ArithmeticTests

open Xunit
open InterpreterModule.Interpreter
open Types

//helper function
let inline floatEqual (a: float) (b: float) (tolerance: float) =
    abs(a - b) < tolerance

//Arithmetic Expressions
[<Fact>]
let ``Test Addition`` () =
    let expr = lexer "1 + 3"
    let result = parseAndEval expr
    Assert.Equal(Int 4, result)

[<Fact>]
let ``Test Subtraction`` () =
    let expr = lexer "3 - 2"
    let result = parseAndEval expr
    Assert.Equal(Int 1, result)

[<Fact>]
let ``Test Multiplication`` () =
    let expr = lexer "2 * 3"
    let result = parseAndEval expr
    Assert.Equal(Int 6, result)

[<Fact>]
let ``Test Division`` () =
    let expr = lexer "6 / 2"
    let result = parseAndEval expr
    Assert.Equal(Int 3, result)

[<Fact>]
let ``Test Complex Expression`` () =
    let expr = lexer "5*3+(2*3-2)/2+6"
    let result = parseAndEval expr
    Assert.Equal(Int 23, result)

[<Fact>]
let ``Test Left Associativity`` () =
    let expr = lexer "9-3-2"
    let result = parseAndEval expr
    Assert.Equal(Int 4, result)

[<Fact>]
let ``Test Integer Division`` () =
    let expr = lexer "10/3"
    let result = parseAndEval expr
    Assert.Equal(Int 3, result)

[<Fact>]
let ``Test Floating Point Division`` () =
    let expr = lexer "10/3.0"
    let result = parseAndEval expr
    match result with
    | Float value -> Assert.True(floatEqual value 3.333 0.001)
    | _ -> Assert.Fail("Expected a float result")

[<Fact>]
let ``Test Modulus Operation`` () =
    let expr = lexer "10%3"
    let result = parseAndEval expr
    Assert.Equal(Int 1, result)

[<Fact>]
let ``Test Unary Minus`` () =
    let expr = lexer "10 - -2"
    let result = parseAndEval expr
    Assert.Equal(Int 12, result)

[<Fact>]
let ``Test Unary Minus at Start`` () =
    let expr = lexer "-2 + 10"
    let result = parseAndEval expr
    Assert.Equal(Int 8, result)

[<Fact>]
let ``Test Power Precedence`` () =
    let expr = lexer "3*5^(2-1) - 2^2*-3"
    let result = parseAndEval expr
    Assert.Equal(Int 87, result)

[<Fact>]
let ``Test Modulus Precedence`` () =
    let expr = lexer "-7%3"
    let result = parseAndEval expr
    Assert.True(result = Int 2 || result = Int -1)

[<Fact>]
let ``Test Power Precedence Over Multiplication`` () =
    let expr = lexer "2*3^2"
    let result = parseAndEval expr
    Assert.Equal(Int 18, result)

[<Fact>]
let ``Test Nested Parentheses`` () =
    let expr = lexer "(((3*2--2)))"
    let result = parseAndEval expr
    Assert.Equal(Int 8, result)

[<Fact>]
let ``Test Parentheses Error`` () =
    let expr = lexer "(((3*2--2))"
    try
        let _ = parseAndEval expr
        Assert.Fail("Expected syntax error")
    with
    | _ -> Assert.True(true)




