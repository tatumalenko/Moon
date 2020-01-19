namespace Moon.Lexer.Tests

module TokenType =
    open Xunit
    open FsUnit.Xunit
    open Moon.Lexer

    [<Theory>]
    [<MemberData("passing_cases")>]
    let ``Given TokenType and valid lexeme, should all match`` token_type lexeme =
        Token.isMatch token_type lexeme |> should equal true

    [<Theory>]
    [<MemberData("failing_cases")>]
    let ``Given TokenType and invalid lexeme, should not match`` token_type lexeme =
        Token.isMatch token_type lexeme |> should equal false

    let passing_cases: seq<obj []> =
        Utils.asTestArguments
            [ [ Letter; "a" ]
              [ Nonzero; "2" ]
              [ Digit; "0" ]
              [ Integer; "8323" ]
              [ Fraction; ".024" ]
              [ Fraction; ".0" ]
              [ Float; "1.1" ]
              [ Alphanum; "a" ]
              [ Id; "a___" ] ]

    let failing_cases: seq<obj []> =
        Utils.asTestArguments
            [ [ Letter; "1" ]
              [ Nonzero; "0" ]
              [ Digit; "a" ]
              [ Integer; "123a" ]
              [ Fraction; "123" ]
              [ Fraction; "1." ]
              [ Float; "1." ]
              [ Alphanum; "?" ]
              [ Id; "__a" ] ]
