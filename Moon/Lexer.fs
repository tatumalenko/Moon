namespace Moon.Lexer

type TokenType =
    | Letter
    | Nonzero
    | Digit
    | Integer
    | Fraction
    | Float
    | Alphanum
    | Id

module Token =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let asString tokenType =
        match tokenType with
        | Letter -> "^[a-zA-Z]$"
        | Nonzero -> "^[1-9]$"
        | Digit -> "^[0-9]$"
        | Integer -> "^(([1-9][0-9]*)|0)$"
        | Fraction -> "^((\.[0-9]*[1-9])|\.0)$"
        | Float -> "^((([1-9][0-9]*)|0)((\.[0-9]*[1-9])|\.0)(e(\+|-)?(([1-9][0-9]*)|0))?)$"
        | Alphanum -> "^([a-zA-Z]|[0-9]|_)$"
        | Id -> "^([a-zA-Z]([a-zA-Z]|[0-9]|_)*)$"

    let asRegex token = Regex(token |> asString)

    let isMatch tokenType lexeme = (asRegex tokenType).IsMatch(lexeme)
