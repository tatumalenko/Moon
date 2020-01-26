module Moon.Lexer

open System.Text.RegularExpressions
open System
open Moon

type TokenType =
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

let rec asString (tokenType: TokenType) =
    match tokenType with
    // Atomic lexical elements
    | Letter -> "[a-zA-Z]"
    | Nonzero -> "[1-9]"
    | Digit -> "[0-9]"
    | IntegerLiteral -> "((" + asString Nonzero + asString Digit + "*)|0)"
    | Fraction -> "((\." + asString Digit + "*" + asString Nonzero + ")|\.0)"
    | PartialFloat ->
        "((" + asString IntegerLiteral + ")(\.)((" + asString IntegerLiteral + ")(" + asString Nonzero
        + "))?(e(\+|-)?)?)"
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

let asRegex tokenType = Regex("^" + (tokenType |> asString) + "$")

let isMatch tokenType lexeme = (asRegex tokenType).IsMatch(lexeme)

let tryMatch (lexeme: string) (tokenType: TokenType) =
    match (isMatch tokenType lexeme) with
    | true -> Some(tokenType, lexeme)
    | false -> None

let matched (lexeme: string): (TokenType * string) list =
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
    |> List.map (tryMatch lexeme)
    |> List.choose id

type Result =
    { token: TokenType
      index: int
      lexeme: string }

[<StructuredFormatDisplay("{display}")>]
type Error =
    { lexeme: string
      line: int
      column: int }
    override m.ToString() = "[LexicalError, " + m.lexeme + ", " + m.line.ToString() + "]"
    member m.display = m.ToString()
    member m.displayDetailed =
        "[Lexical Error (line " + m.line.ToString() + ", column " + m.column.ToString() + "): \"" + m.lexeme + "\"]"

[<StructuredFormatDisplay("{display}")>]
type TokenResult =
    { token: TokenType
      lexeme: string
      line: int
      column: int }
    override m.ToString() = "[" + m.token.ToString() + ", " + m.lexeme + ", " + m.line.ToString() + "]"
    member m.display = m.ToString()

type Outcome =
    | Result of TokenResult
    | Error of Error

let createResult (result: Result) (line: int) (column: int) =
    match result.token with
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
            { token = result.token
              lexeme = result.lexeme
              line = line
              column = column }
    | _ ->
        Error
            { lexeme = result.lexeme
              line = line
              column = column }

let tokenizeChars (stream: char list) =
    let mutable index = 0
    let mutable strBuffer = stream.GetSlice(Some 0, Some index)
    let mutable keepLooking = true
    let mutable found = []
    let mutable result: Result option = None

    while keepLooking = true do
        let newResult = matched (strBuffer |> String.Concat)
        keepLooking <- (List.length newResult) > 0 && index + 1 <= (List.length stream) - 1

        if keepLooking = true then
            found <- newResult
            index <- index + 1
            strBuffer <- stream.GetSlice(Some 0, Some index)
        else
            if List.length newResult > 0 then found <- newResult
            if List.length found > 0 then
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
    let mutable tokens = []
    let mutable results = []

    for (row, line) in List.mapi (fun i e -> (i, e)) stream do
        let mutable chars = List.ofSeq line
        let mutable column: int = 0

        while column + 1 < List.length chars do
            let token = tokenizeChars (chars.GetSlice(Some column, None))

            do match token with
               | Some result ->
                   column <-
                       column + (if result.index > 0 then result.index
                                 else 1)
                   tokens <- tokens @ [ token ]
                   results <- results @ [ (createResult result (row + 1) column) ]
               | None -> column <- column + 1

    results

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

let display (tokenResults: Outcome list): string =
    let mutable str = ""

    let mutable currentLine =
        match tokenResults.[0] with
        | Result tr -> tr.line
        | Error e -> e.line

    for tokenResult in tokenResults do
        let line =
            match tokenResult with
            | Result tr -> tr.line
            | Error e -> e.line

        let tokenResultAsString =
            match tokenResult with
            | Result tr -> sprintf "%A" tr
            | Error e -> sprintf "%A" e

        if line = currentLine then
            str <-
                if str = "" then tokenResultAsString
                else str + " " + tokenResultAsString
        else
            currentLine <- line
            str <- str + "\n" + tokenResultAsString
    str

let writeTokens (outcomes: Outcome list) (path: string option) =
    Utils.write (display outcomes) path

let writeErrors (outcomes: Outcome list) (path: string option) =
    Utils.write
        (lexicalErrorsFromOutcomes outcomes
         |> List.map (fun e -> e.displayDetailed)
         |> List.fold (fun state e ->
             if state = "" then e
             else state + "\n" + e) "") path
