module Moon.Tests.Lexer

open FsUnit.Xunit
open Moon
open Moon.Tests
open System.Text.RegularExpressions
open Xunit

[<Theory>]
[<MemberData("validCases")>]
let ``Given Token and valid lexeme, should all match`` tokenType lexeme =
    let a =
        Lexer.tokenized ";\n"

    let p = SemiColon.pattern
    let r = Regex.IsMatch(";\n", p, RegexOptions.ExplicitCapture)
    let r2 = Regex.IsMatch(";\n", @"\A[\s\S]*\z", RegexOptions.ExplicitCapture)
    Lexer.isMatch tokenType lexeme |> should equal true

[<Theory>]
[<MemberData("invalidCases")>]
let ``Given Token and invalid lexeme, should not match`` tokenType lexeme =
    Lexer.isMatch tokenType lexeme |> should equal false

[<Theory>]
[<MemberData("tokenizedCases")>]
let ``Given lexeme, should tokenize to given Token`` token lexeme =
    let tokenized = Lexer.tokenized lexeme
    tokenized |> should equal token

[<Fact>]
let ``Given text with inline comment, then properly tokenized``() =
    let text = "\n\
        // This is a line comment\n\
        int id = 2 + 3;\n\
        "

    let outcomes = Lexer.tokenize (Text text)

    let expected: Token list =
        [ { tokenType =
                LineComment "// This is a line comment\n"
            location =
                { line = 2
                  column = 1 } }
          { tokenType = Id "int"
            location =
                { line = 3
                  column = 1 } }
          { tokenType = Id "id"
            location =
                { line = 3
                  column = 5 } }
          { tokenType = Equal
            location =
                { line = 3
                  column = 8 } }
          { tokenType = IntegerLiteral "2"
            location =
                { line = 3
                  column = 10 } }
          { tokenType = Plus
            location =
                { line = 3
                  column = 12 } }
          { tokenType = IntegerLiteral "3"
            location =
                { line = 3
                  column = 14 } }
          { tokenType = SemiColon
            location =
                { line = 3
                  column = 15 } } ]

    outcomes |> should equal expected

[<Fact>]
let ``Given text with single line block comment, then properly tokenized``() =
    let text = "\n\
        /* This is a line comment */\n\
        int id = 2 + 3;\n\
        "

    let outcomes = Lexer.tokenize (Text text)

    let expected =
        [ { tokenType = BlockComment "/* This is a line comment */"
            location =
                { line = 2
                  column = 1 } }
          { tokenType = Id "int"
            location =
                { line = 3
                  column = 1 } }
          { tokenType = Id "id"
            location =
                { line = 3
                  column = 5 } }
          { tokenType = Equal
            location =
                { line = 3
                  column = 8 } }
          { tokenType = IntegerLiteral "2"
            location =
                { line = 3
                  column = 10 } }
          { tokenType = Plus
            location =
                { line = 3
                  column = 12 } }
          { tokenType = IntegerLiteral "3"
            location =
                { line = 3
                  column = 14 } }
          { tokenType = SemiColon
            location =
                { line = 3
                  column = 15 } } ]

    outcomes |> should equal expected

[<Fact>]
let ``Given text with multi line block comment, then properly tokenized``() =
    let text = @"
/*
  This is
  a
  block comment
*/

int my_var = 2 + 3;

// This is a line comment
0.234 + 1234 == 1234.234;

read(my_var);
list (a,b,c);
"

    let tokens = Lexer.tokenize (InputType.Text text)

    let expected =
        [ { tokenType =
                BlockComment "/*\n  This is\n  a\n  block comment\n*/"
            location =
                { line = 2
                  column = 1 } }
          { tokenType = Id "int"
            location =
                { line = 8
                  column = 1 } }
          { tokenType = Id "my_var"
            location =
                { line = 8
                  column = 5 } }
          { tokenType = Equal
            location =
                { line = 8
                  column = 12 } }
          { tokenType = IntegerLiteral "2"
            location =
                { line = 8
                  column = 14 } }
          { tokenType = Plus
            location =
                { line = 8
                  column = 16 } }
          { tokenType = IntegerLiteral "3"
            location =
                { line = 8
                  column = 18 } }
          { tokenType = SemiColon
            location =
                { line = 8
                  column = 19 } }
          { tokenType =
                LineComment "// This is a line comment\n"
            location =
                { line = 10
                  column = 1 } }
          { tokenType = FloatLiteral "0.234"
            location =
                { line = 11
                  column = 1 } }
          { tokenType = Plus
            location =
                { line = 11
                  column = 7 } }
          { tokenType = IntegerLiteral "1234"
            location =
                { line = 11
                  column = 9 } }
          { tokenType = EqualEqual
            location =
                { line = 11
                  column = 14 } }
          { tokenType = FloatLiteral "1234.234"
            location =
                { line = 11
                  column = 17 } }
          { tokenType = SemiColon
            location =
                { line = 11
                  column = 25 } }
          { tokenType = Read
            location =
                { line = 13
                  column = 1 } }
          { tokenType = OpenBracket
            location =
                { line = 13
                  column = 5 } }
          { tokenType = Id "my_var"
            location =
                { line = 13
                  column = 6 } }
          { tokenType = ClosedBracket
            location =
                { line = 13
                  column = 12 } }
          { tokenType = SemiColon
            location =
                { line = 13
                  column = 13 } }
          { tokenType = Id "list"
            location =
                { line = 14
                  column = 1 } }
          { tokenType = OpenBracket
            location =
                { line = 14
                  column = 6 } }
          { tokenType = Id "a"
            location =
                { line = 14
                  column = 7 } }
          { tokenType = Comma
            location =
                { line = 14
                  column = 8 } }
          { tokenType = Id "b"
            location =
                { line = 14
                  column = 9 } }
          { tokenType = Comma
            location =
                { line = 14
                  column = 10 } }
          { tokenType = Id "c"
            location =
                { line = 14
                  column = 11 } }
          { tokenType = ClosedBracket
            location =
                { line = 14
                  column = 12 } }
          { tokenType = SemiColon
            location =
                { line = 14
                  column = 13 } } ]

//    Utils.write
//        (tokens
//         |> List.map (fun e -> show e)
//         |> String.concat "\n") (Utils.makePath "tokens.txt")
//    Utils.write
//        (expected
//         |> List.map (fun e -> show e)
//         |> String.concat "\n") (Utils.makePath "expected.txt")

    tokens |> should equal expected

//[<Fact>]
//let ``Given file path with positive grading, contents properly tokenized``() =
//    let outcomes = tokenize (FilePath(Utils.makePath "resources/lexer/in/positivegrading.src"))
//    writeTokens outcomes (Utils.makePath "resources/lexer/out/positivegrading.outlextokens")
//    writeErrors outcomes (Utils.makePath "resources/lexer/out/positivegrading.outlexerrors")
//
//    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/positivegrading.outlextokens")
//    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/positivegrading.outlexerrors")
//
//    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/positivegrading.outlextokens")
//    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/positivegrading.outlexerrors")
//
//    actualTokens |> should matchList expectedTokens
//    actualErrors |> should matchList expectedErrors

//[<Fact>]
//let ``Given file path with negative grading, contents properly tokenized with errors``() =
//    let outcomes = tokenize (FilePath(Utils.makePath "resources/lexer/in/negativegrading.src"))
//    writeTokens outcomes (Utils.makePath "resources/lexer/out/negativegrading.outlextokens")
//    writeErrors outcomes (Utils.makePath "resources/lexer/out/negativegrading.outlexerrors")
//
//    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/negativegrading.outlextokens")
//    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/negativegrading.outlexerrors")
//
//    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/negativegrading.outlextokens")
//    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/negativegrading.outlexerrors")
//
//    actualTokens |> should matchList expectedTokens
//    actualErrors |> should matchList expectedErrors
//
//[<Fact>]
//let ``Given file path with various test cases (testcase1.src), contents properly tokenized with errors``() =
//    let outcomes = tokenize (FilePath(Utils.makePath "resources/lexer/in/testcase1.src"))
//    writeTokens outcomes (Utils.makePath "resources/lexer/out/testcase1.outlextokens")
//    writeErrors outcomes (Utils.makePath "resources/lexer/out/testcase1.outlexerrors")
//
//    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/testcase1.outlextokens")
//    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/testcase1.outlexerrors")
//
//    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/testcase1.outlextokens")
//    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/testcase1.outlexerrors")
//
//    actualTokens |> should matchList expectedTokens
//    actualErrors |> should matchList expectedErrors
//
//[<Fact>]
//let ``Given file path with various test cases (testcase2.src), contents properly tokenized with errors``() =
//    let outcomes = tokenize (FilePath(Utils.makePath "resources/lexer/in/testcase2.src"))
//    writeTokens outcomes (Utils.makePath "resources/lexer/out/testcase2.outlextokens")
//    writeErrors outcomes (Utils.makePath "resources/lexer/out/testcase2.outlexerrors")
//
//    let actualTokens = Utils.read (Utils.makePath "resources/lexer/out/testcase2.outlextokens")
//    let actualErrors = Utils.read (Utils.makePath "resources/lexer/out/testcase2.outlexerrors")
//
//    let expectedTokens = Utils.read (Utils.makePath "resources/lexer/expected/testcase2.outlextokens")
//    let expectedErrors = Utils.read (Utils.makePath "resources/lexer/expected/testcase2.outlexerrors")
//
//    actualTokens |> should matchList expectedTokens
//    actualErrors |> should matchList expectedErrors

let validCases =
    Utils.asTestArguments
        [ [ LineComment "// This is a line comment\n"
            "// This is a line comment\n" ]
          [ Letter "a"
            "a" ]
          [ Nonzero "2"
            "2" ]
          [ Digit "0"
            "0" ]
          [ IntegerLiteral "8323"
            "8323" ]
          [ Fraction ".024"
            ".024" ]
          [ Fraction ".0"
            ".0" ]
          [ FloatLiteral "1.1"
            "1.1" ]
          [ Alphanum "a"
            "a" ]
          [ Id "a___"
            "a___" ]
          [ BlockComment "/**/"
            "/**/" ]
          [ BlockComment "/* */"
            "/* */" ]
          [ BlockComment "/* This is a block comment */"
            "/* This is a block comment */" ]
          [ BlockComment "/*\n This is a block comment\n*/"
            "/*\n This is a block comment\n*/" ]
          [ BlockComment "/*\n This is a block comment\n     */"
            "/*\n This is a block comment\n     */" ]
          [ Invalid "/*\n This is a block comment\n     */\n"
            "/*\n This is a block comment\n     */\n" ]
          [ SlashAsterisk; "/*" ]
          [ PartialBlockComment "/* "
            "/* " ]
          [ PartialBlockComment "/* \n"
            "/* \n" ]
          [ PartialBlockComment "/* \n\n "
            "/* \n\n " ]
          [ PartialBlockComment "/*\n This is a partial block comment\n"
            "/*\n This is a partial block comment\n" ]
          [ PartialBlockComment @"/*
            This is a
            partial
            !@#$@!$~/** /
            block comment"
            @"/*
            This is a
            partial
            !@#$@!$~/** /
            block comment" ]
          [ Newline; "\n" ]
          [ SemiColon; ";" ]
          [ Invalid ";\n"
            ";\n" ] ]

let invalidCases =
    Utils.asTestArguments
        [ [ Letter "1"
            "1" ]
          [ Nonzero "0"
            "0" ]
          [ Digit "a"
            "a" ]
          [ IntegerLiteral "123a"
            "123a" ]
          [ Fraction "123"
            "123" ]
          [ Fraction "1."
            "1." ]
          [ FloatLiteral "1."
            "1." ]
          [ Alphanum "?"
            "?" ]
          [ Id "__a"
            "__a" ] ]

let tokenizedCases =
    Utils.asTestArguments
        [ [ Invalid "/*\n This is a partial block comment*/\n"
            "/*\n This is a partial block comment*/\n" ]
          [ LineComment "// This is a line comment\n"
            "// This is a line comment\n" ]
          [ Id "a"
            "a" ]
          [ Id "a___"
            "a___" ]
          [ IntegerLiteral "2"
            "2" ]
          [ IntegerLiteral "0"
            "0" ]
          [ IntegerLiteral "8323"
            "8323" ]
          [ Fraction ".024"
            ".024" ]
          [ Fraction ".0"
            ".0" ]
          [ FloatLiteral "1.1"
            "1.1" ]
          [ BlockComment "/**/"
            "/**/" ]
          [ BlockComment "/* */"
            "/* */" ]
          [ BlockComment "/* This is a block comment */"
            "/* This is a block comment */" ]
          [ BlockComment "/*\n This is a block comment\n*/"
            "/*\n This is a block comment\n*/" ]
          [ BlockComment "/*\n This is a block comment\n     */"
            "/*\n This is a block comment\n     */" ]
          [ Invalid "/*\n This is a block comment\n     */\n"
            "/*\n This is a block comment\n     */\n" ]
          [ PartialBlockComment "/*"
            "/*" ]
          [ PartialBlockComment "/* "
            "/* " ]
          [ PartialBlockComment "/* \n"
            "/* \n" ]
          [ PartialBlockComment "/* \n\n "
            "/* \n\n " ]
          [ PartialBlockComment "/*\n This is a partial block comment\n"
            "/*\n This is a partial block comment\n" ]
          [ PartialBlockComment @"/*
            This is a
            partial
            !@#$@!$~/** /
            block comment"
            @"/*
            This is a
            partial
            !@#$@!$~/** /
            block comment" ]
          [ Invalid "/*\n This is a partial block comment*/\n"
            "/*\n This is a partial block comment*/\n" ]
          [ Invalid "/*\n This is a partial block comment*/ "
            "/*\n This is a partial block comment*/ " ]
          [ Newline; "\n" ]
          [ SemiColon; ";" ]
          [ Invalid ";\n"
            ";\n" ] ]
