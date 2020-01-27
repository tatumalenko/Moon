module Moon.Lexer

open System.Text.RegularExpressions
open System
open Moon

type Token =
    // Atomic lexical elements
    | Letter
    | Nonzero
    | Digit
    | IntegerLiteral
    | Fraction
    | PartialFloat
    | FloatLiteral
    | Alphanum
    | Id
    // Operators (single character)
    | Equal // =
    | Plus // +
    | Minus // -
    | Asterisk // *
    | Lt // <
    | Gt // >
    | Colon // :
    | Slash // /
    | SemiColon // ;
    | Comma // ,
    | Period // .
    | OpenBracket // (
    | ClosedBracket // )
    | OpenSquareBracket // [
    | ClosedSquareBracket // ]
    | OpenBrace // {
    | ClosedBrace // }
    // Operators (double character)
    | EqualEqual // ==
    | LtEqual // <=
    | GtEqual // >=
    | LtGt // <>
    | ColonColon // ::
    | SlashSlash // //
    | SlashAsterisk // /*
    | AsteriskSlash // */
    // Keywords
    | If
    | Then
    | Else
    | While
    | Class
    | Integer
    | Float
    | Do
    | End
    | Public
    | Private
    | Or
    | And
    | Not
    | Read
    | Write
    | Return
    | Main
    | Inherits
    | Local
    // Invalid (anything else)
    | Invalid

type LexicalError =
    | InvalidNumber
    | InvalidCharacter

let rec asString (token: Token) =
    match token with
    // Atomic lexical elements
    | Letter -> "[a-zA-Z]"
    | Nonzero -> "[1-9]"
    | Digit -> "[0-9]"
    | IntegerLiteral -> "((" + asString Nonzero + asString Digit + "*)|0)"
    | Fraction -> "((\." + asString Digit + "*" + asString Nonzero + ")|\.0)"
    | PartialFloat ->
        "((" + asString IntegerLiteral + ")(\.)(" + asString Digit + "*" + ")(e(\+|-)?)?)"
    | FloatLiteral ->
        "((" + asString IntegerLiteral + ")(" + asString Fraction + ")(e(\+|-)?(" + asString IntegerLiteral + "))?)"
    | Alphanum -> "(" + asString Letter + "|" + asString Digit + "|_)"
    | Id -> "(" + asString Letter + "(" + asString Alphanum + ")*)"
    // Operators (single character)
    | Equal -> "="
    | Plus -> "\+"
    | Minus -> "-"
    | Asterisk -> "\*"
    | Lt -> "<"
    | Gt -> ">"
    | Colon -> ":"
    | Slash -> "/"
    | SemiColon -> ";"
    | Comma -> ","
    | Period -> "\."
    | OpenBracket -> "\("
    | ClosedBracket -> "\)"
    | OpenSquareBracket -> "\["
    | ClosedSquareBracket -> "\]"
    | OpenBrace -> "\{"
    | ClosedBrace -> "\}"
    // Operators (double character)
    | EqualEqual -> "=="
    | LtEqual -> "<="
    | GtEqual -> ">="
    | LtGt -> "<>"
    | ColonColon -> "::"
    | SlashSlash -> "//"
    | SlashAsterisk -> "/\*"
    | AsteriskSlash -> "\*/"
    // Keywords
    | If -> "if"
    | Then -> "then"
    | Else -> "else"
    | While -> "while"
    | Class -> "class"
    | Integer -> "integer"
    | Float -> "float"
    | Do -> "do"
    | End -> "end"
    | Public -> "public"
    | Private -> "private"
    | Or -> "or"
    | And -> "and"
    | Not -> "not"
    | Read -> "read"
    | Write -> "write"
    | Return -> "return"
    | Main -> "main"
    | Inherits -> "inherits"
    | Local -> "local"
    | Invalid -> ".*"

let asRegex token = Regex("^" + (token |> asString) + "$")

let isMatch token lexeme = (asRegex token).IsMatch(lexeme)

let tryMatch (lexeme: string) (token: Token) =
    match (isMatch token lexeme) with
    | true -> Some(token, lexeme)
    | false -> None

let precedenceHighToLowTokens =
    [ Equal
      Plus
      Minus
      Asterisk
      Lt
      Gt
      Colon
      Slash
      SemiColon
      Comma
      Period
      OpenBracket
      ClosedBracket
      OpenSquareBracket
      ClosedSquareBracket
      OpenBrace
      ClosedBrace
      EqualEqual
      LtEqual
      GtEqual
      LtGt
      ColonColon
      SlashSlash
      SlashAsterisk
      AsteriskSlash
      If
      Then
      Else
      While
      Class
      Integer
      Float
      Do
      End
      Public
      Private
      Or
      And
      Not
      Read
      Write
      Return
      Main
      Inherits
      Local
      Id
      FloatLiteral
      IntegerLiteral
      Letter
      Nonzero
      Digit
      Fraction
      PartialFloat
      Alphanum ]

let partialTokens =
    [ Letter; Nonzero; Digit; Fraction; PartialFloat; Alphanum ]

let matched (lexeme: string): (Token * string) list =
    precedenceHighToLowTokens
    |> List.map (tryMatch lexeme)
    |> List.choose id

type PartialResult =
    { token: Token
      index: int
      lexeme: string }

[<StructuredFormatDisplay("{display}")>]
type Error =
    { kind: LexicalError
      lexeme: string
      line: int
      column: int }
    override m.ToString() = "[" + m.kind.ToString() + ", " + m.lexeme + ", " + m.line.ToString() + "]"
    member m.display = m.ToString()
    member m.displayDetailed =
        "[LexicalError (" + m.kind.ToString() + ", line " + m.line.ToString() + ", column " + m.column.ToString()
        + "): \"" + m.lexeme + "\"]"

[<StructuredFormatDisplay("{display}")>]
type Result =
    { token: Token
      lexeme: string
      line: int
      column: int }
    override m.ToString() = "[" + m.token.ToString() + ", " + m.lexeme + ", " + m.line.ToString() + "]"
    member m.display = m.ToString()

type Outcome =
    | Result of Result
    | Error of Error

let makeOutcome (partialResult: PartialResult) (line: int) (column: int) =
    match partialResult.token with
    // Operators (single character)
    | Equal // =
    | Plus // +
    | Minus // -
    | Asterisk // *
    | Lt // <
    | Gt // >
    | Colon // :
    | Slash // /
    | SemiColon // ;
    | Comma // ,
    | Period // .
    | OpenBracket // (
    | ClosedBracket // )
    | OpenSquareBracket // [
    | ClosedSquareBracket // ]
    | OpenBrace // {
    | ClosedBrace // }
    | EqualEqual // ==
    | LtEqual // <=
    | GtEqual // >=
    | LtGt // <>
    | ColonColon // ::
    | SlashSlash // //
    | SlashAsterisk
    | AsteriskSlash
    | If
    | Then
    | Else
    | While
    | Class
    | Integer
    | Float
    | Do
    | End
    | Public
    | Private
    | Or
    | And
    | Not
    | Read
    | Write
    | Return
    | Main
    | Inherits
    | Local
    | Id
    | FloatLiteral
    | IntegerLiteral ->
        Result
            { token = partialResult.token
              lexeme = partialResult.lexeme
              line = line
              column = column }
    | Fraction
    | PartialFloat ->
        Error
            { kind = InvalidNumber
              lexeme = partialResult.lexeme
              line = line
              column = column }
    | Invalid
    | _ ->
        Error
            { kind = InvalidCharacter
              lexeme = partialResult.lexeme
              line = line
              column = column }

let tokenizeChars (stream: char list) =
    let mutable index = 0
    let mutable strBuffer = stream.GetSlice(Some 0, Some index)
    let mutable keepLooking = true
    let mutable found = []
    let mutable result: PartialResult option = None
    let isPartialToken = fun token -> List.contains token partialTokens

    while keepLooking = true do
        let newResult = matched (strBuffer |> String.Concat)
        keepLooking <- (List.length newResult) > 0 && index + 1 <= (List.length stream) - 1

        if keepLooking = true then
            found <- newResult
            let (token, lexeme) = newResult.[0]
            if not (isPartialToken token) then
                result <-
                    Some
                        { token = token
                          index = index
                          lexeme = lexeme }
            index <- index + 1
            strBuffer <- stream.GetSlice(Some 0, Some index)
        else
            if List.length newResult > 0 then
                found <- newResult
                // Here the first element (index 0) has highest priority on token type
                let (token, lexeme) = newResult.[0]
                if not (isPartialToken token) then
                    result <-
                        Some
                            { token = token
                              index = index
                              lexeme = lexeme }
            if not (result = None) then
                result <- result
            else if List.length found > 0 then
                // Here the first element (index 0) has highest priority on token type
                let (token, lexeme) = found.[0]
                result <-
                    Some
                        { token = token
                          index = index
                          lexeme = lexeme }
            else if List.forall (fun e -> (strBuffer |> String.Concat) <> e) [ " "; "\t" ] then
                result <-
                    Some
                        { token = Invalid
                          index = index
                          lexeme = (strBuffer |> String.Concat) }
            else
                result <- None

    result

let tokenizeStrings (stream: string list): Outcome list =
    let mutable outcomes = []

    for (row, line) in List.mapi (fun i e -> (i, e)) stream do
        let mutable chars = List.ofSeq line
        let mutable column: int = 0

        while column < List.length chars do
            let token = tokenizeChars (chars.GetSlice(Some column, None))

            match token with
            | Some result ->
                column <-
                    column + (if result.index > 0 then result.lexeme.Length
                              else 1)
                outcomes <- outcomes @ [ (makeOutcome result (row + 1) column) ]
            | None -> column <- column + 1

    outcomes

let tokenizeFile (filePath: string): Outcome list =
    tokenizeStrings (Utils.read filePath)

type InputType =
    | FilePath of string
    | Text of string list

let tokenize (input: InputType): Outcome list =
    match input with
    | FilePath path -> tokenizeFile path
    | Text stream -> tokenizeStrings stream

let lexicalErrorsFromOutcomes (outcomes: Outcome list): Error list =
    let errorChooser =
        fun e ->
            match e with
            | Error e -> Some e
            | _ -> None
    List.choose errorChooser outcomes

let display (outcomes: Outcome list): string =
    let mutable str = ""

    let mutable currentLine =
        match outcomes.[0] with
        | Result tr -> tr.line
        | Error e -> e.line

    for outcome in outcomes do
        let line =
            match outcome with
            | Result tr -> tr.line
            | Error e -> e.line

        let outcomeAsString =
            match outcome with
            | Result tr -> sprintf "%A" tr
            | Error e -> sprintf "%A" e

        if line = currentLine then
            str <-
                if str = "" then outcomeAsString
                else str + " " + outcomeAsString
        else
            currentLine <- line
            str <- str + "\n" + outcomeAsString
    str

let writeTokens (outcomes: Outcome list) (path: string) =
    Utils.write (display outcomes) path

let writeErrors (outcomes: Outcome list) (path: string) =
    Utils.write
        (lexicalErrorsFromOutcomes outcomes
         |> List.map (fun e -> e.displayDetailed)
         |> List.fold (fun state e ->
             if state = "" then e
             else state + "\n" + e) "") path
