module Moon.Tests.Lexer

open Xunit
open FsUnit.Xunit
open Moon
open Moon.Lexer
open Moon.Tests

[<Theory>]
[<MemberData("validCases")>]
let ``Given TokenType and valid lexeme, should all match`` tokenType lexeme =
    isMatch tokenType lexeme |> should equal true

[<Theory>]
[<MemberData("invalidCases")>]
let ``Given TokenType and invalid lexeme, should not match`` tokenType lexeme =
    isMatch tokenType lexeme |> should equal false

[<Fact>]
let ``Given file path with positive grading, contents properly tokenized``() =
    let outcomes = tokenize (FilePath (Utils.makePath "resources/lexer/in/positivegrading.src")) 
    writeTokens outcomes (Some (Utils.makePath "resources/lexer/out/positivegrading.outlextokens"))
    writeErrors outcomes (Some (Utils.makePath "resources/lexer/out/positivegrading.outlexerrors"))
    
    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/positivegrading.outlextokens")
    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/positivegrading.outlexerrors")
    
    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/positivegrading.outlextokens")
    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/positivegrading.outlexerrors")
    
    actualTokens |> should matchList expectedTokens
    actualErrors |> should matchList expectedErrors
    
[<Fact>]
let ``Given file path with negative grading, contents properly tokenized with errors``() =
    let outcomes = tokenize (FilePath (Utils.makePath "resources/lexer/in/negativegrading.src"))
    writeTokens outcomes (Some (Utils.makePath "resources/lexer/out/negativegrading.outlextokens"))
    writeErrors outcomes (Some (Utils.makePath "resources/lexer/out/negativegrading.outlexerrors"))
    
    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/negativegrading.outlextokens")
    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/negativegrading.outlexerrors")
    
    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/negativegrading.outlextokens")
    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/negativegrading.outlexerrors")
    
    actualTokens |> should matchList expectedTokens
    actualErrors |> should matchList expectedErrors

[<Fact>]
let ``Given file path with various test cases (testcase1.src), contents properly tokenized with errors``() =
    let outcomes = tokenize (FilePath (Utils.makePath "resources/lexer/in/testcase1.src"))
    writeTokens outcomes (Some (Utils.makePath "resources/lexer/out/testcase1.outlextokens"))
    writeErrors outcomes (Some (Utils.makePath "resources/lexer/out/testcase1.outlexerrors"))
    
    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/testcase1.outlextokens")
    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/testcase1.outlexerrors")
    
    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/testcase1.outlextokens")
    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/testcase1.outlexerrors")
    
    actualTokens |> should matchList expectedTokens
    actualErrors |> should matchList expectedErrors
    
[<Fact>]
let ``Given file path with various test cases (testcase2.src), contents properly tokenized with errors``() =
    let outcomes = tokenize (FilePath (Utils.makePath "resources/lexer/in/testcase2.src"))
    writeTokens outcomes (Some (Utils.makePath "resources/lexer/out/testcase2.outlextokens"))
    writeErrors outcomes (Some (Utils.makePath "resources/lexer/out/testcase2.outlexerrors"))
    
    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/testcase2.outlextokens")
    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/testcase2.outlexerrors")
    
    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/testcase2.outlextokens")
    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/testcase2.outlexerrors")
    
    actualTokens |> should matchList expectedTokens
    actualErrors |> should matchList expectedErrors
    
let validCases =
    Utils.asTestArguments
        [ [ Letter; "a" ]
          [ Nonzero; "2" ]
          [ Digit; "0" ]
          [ IntegerLiteral; "8323" ]
          [ Fraction; ".024" ]
          [ Fraction; ".0" ]
          [ FloatLiteral; "1.1" ]
          [ Alphanum; "a" ]
          [ Id; "a___" ] ]

let invalidCases =
    Utils.asTestArguments
        [ [ Letter; "1" ]
          [ Nonzero; "0" ]
          [ Digit; "a" ]
          [ IntegerLiteral; "123a" ]
          [ Fraction; "123" ]
          [ Fraction; "1." ]
          [ FloatLiteral; "1." ]
          [ Alphanum; "?" ]
          [ Id; "__a" ] ]