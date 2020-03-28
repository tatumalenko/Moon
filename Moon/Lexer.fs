namespace Moon

open System
open System.Text.RegularExpressions

[<StructuredFormatDisplay("line={line}, column={column}")>]
type Location =
    { line: int
      column: int }

and InputType =
    | FilePath of string
    | Text of string

and [<StructuredFormatDisplay("{show}")>] Token =
    { tokenType: TokenType
      location: Location }
    member x.case = x.tokenType.case
    member x.lexeme = show x.tokenType
    member x.isPartial = x.tokenType.isPartial
    member x.isInvalid = x.tokenType.isInvalid
    member x.isIgnore = x.tokenType.isIgnore
    member x.isFinal = x.tokenType.isFinal
    member x.isValid = x.tokenType.isValid
    member x.show = sprintf "[%s ('%s'), %s]" x.case (x.lexeme.Replace("\n", "").Replace("\r", "")) (show x.location)

and [<StructuredFormatDisplay("{show}")>] TokenType =
    | Space
    | Newline
    | LineComment of string
    | PartialLineComment of string
    | BlockComment of string
    | PartialBlockComment of string
    // Atomic lexical elements
    | Letter of string
    | Nonzero of string
    | Digit of string
    | IntegerLiteral of string
    | Fraction of string
    | PartialFloat of string
    | FloatLiteral of string
    | Alphanum of string
    | Id of string
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
    | Void
    // Invalid (anything else)
    | Invalid of string
    | A
    | B
    | C
    | D

    member x.show =
        match x with
        | Space -> " "
        | Newline -> "\n"
        | LineComment lexeme -> lexeme
        | PartialLineComment lexeme -> lexeme
        | BlockComment lexeme -> lexeme
        | PartialBlockComment lexeme -> lexeme
        // Atomic lexical elements
        | Letter lexeme -> lexeme
        | Nonzero lexeme -> lexeme
        | Digit lexeme -> lexeme
        | IntegerLiteral lexeme -> lexeme
        | Fraction lexeme -> lexeme
        | PartialFloat lexeme -> lexeme
        | FloatLiteral lexeme -> lexeme
        | Alphanum lexeme -> lexeme
        | Id lexeme -> lexeme
        // Operators (single character)
        | Equal -> "="
        | Plus -> "+"
        | Minus -> "-"
        | Asterisk -> "*"
        | Lt -> "<"
        | Gt -> ">"
        | Colon -> ":"
        | Slash -> "/"
        | SemiColon -> ";"
        | Comma -> ","
        | Period -> "."
        | OpenBracket -> "("
        | ClosedBracket -> ")"
        | OpenSquareBracket -> "["
        | ClosedSquareBracket -> "]"
        | OpenBrace -> "{"
        | ClosedBrace -> "}"
        // Operators (double character)
        | EqualEqual -> "=="
        | LtEqual -> "<="
        | GtEqual -> ">="
        | LtGt -> "<>"
        | ColonColon -> "::"
        | SlashSlash -> "//"
        | SlashAsterisk -> "/*"
        | AsteriskSlash -> "*/"
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
        | Invalid lexeme -> lexeme
        | Void -> "void"
        | A -> "a"
        | B -> "b"
        | C -> "c"
        | D -> "d"

    member x.case = Utils.unionCaseName x

    static member fromString (text: String): Result<TokenType, string> =
        match text with
        | "id"
        | "'id'" -> Ok(Id "Id")
        | "class"
        | "'class'" -> Ok Class
        | "integerliteral"
        | "'intNum'" -> Ok(IntegerLiteral "IntegerLiteral")
        | "floatliteral"
        | "'floatNum'" -> Ok(FloatLiteral "FloatLiteral")
        | "if"
        | "'if'" -> Ok If
        | "then"
        | "'then'" -> Ok Then
        | "else"
        | "'else'" -> Ok Else
        | "while"
        | "'while'" -> Ok While
        | "equal"
        | "'='"
        | "eq" -> Ok Equal
        | "period"
        | "dot"
        | "'.'" -> Ok Period
        | "comma"
        | "','" -> Ok Comma
        | "semicolon"
        | "semi"
        | "';'" -> Ok SemiColon
        | "colon"
        | "':'" -> Ok Colon
        | "coloncolon"
        | "'sr'" -> Ok ColonColon
        | "plus"
        | "'+'" -> Ok Plus
        | "minus"
        | "'-'" -> Ok Minus
        | "asterisk"
        | "mult"
        | "'*'" -> Ok Asterisk
        | "slash"
        | "div"
        | "'/'" -> Ok Slash
        | "or"
        | "'or'" -> Ok Or
        | "and"
        | "'and'" -> Ok And
        | "do"
        | "'do'" -> Ok Do
        | "end"
        | "'end'" -> Ok End
        | "not"
        | "'not'" -> Ok Not
        | "opensquarebracket"
        | "lsqbr"
        | "'['" -> Ok OpenSquareBracket
        | "closedsquarebracket"
        | "rsqbr"
        | "']'" -> Ok ClosedSquareBracket
        | "openbrace"
        | "lcurbr"
        | "'{'" -> Ok OpenBrace
        | "closedbrace"
        | "rcurbr"
        | "'}'" -> Ok ClosedBrace
        | "openbracket"
        | "lpar"
        | "'('" -> Ok OpenBracket
        | "closedbracket"
        | "rpar"
        | "')'" -> Ok ClosedBracket
        | "inherits"
        | "'inherits'" -> Ok Inherits
        | "local"
        | "'local'" -> Ok Local
        | "main"
        | "'main'" -> Ok Main
        | "equalequal"
        | "eqeq"
        | "'=='" -> Ok EqualEqual
        | "ltgt"
        | "neq"
        | "'<>'" -> Ok LtGt
        | "gtequal"
        | "geq"
        | "'>='" -> Ok GtEqual
        | "gt"
        | "'>'" -> Ok Gt
        | "ltequal"
        | "leq"
        | "'<='" -> Ok LtEqual
        | "lt"
        | "'<'" -> Ok Lt
        | "read"
        | "'read'" -> Ok Read
        | "write"
        | "'write'" -> Ok Write
        | "return"
        | "'return'" -> Ok Return
        | "float"
        | "'float'" -> Ok Float
        | "integer"
        | "'integer'" -> Ok Integer
        | "void"
        | "'void'" -> Ok Void
        | "private"
        | "'private'" -> Ok Private
        | "public"
        | "'public'" -> Ok Public
        | symbol -> Error("Invalid token: " + symbol)

    member x.isPartial =
        match x with
        | Letter _
        | Nonzero _
        | Digit _
        | Fraction _
        | PartialFloat _
        | Alphanum _ -> true
        | _ -> false

    member x.isInvalid =
        match x with
        | Invalid _
        | A
        | B
        | C
        | D -> true
        | _ -> false

    member x.isIgnore = x = Space || x = Newline
    member x.isFinal = not x.isPartial && not x.isInvalid && not x.isIgnore
    member x.isValid = x.isPartial || x.isFinal
    member x.pattern =
        let rec asPattern (token: TokenType) =
            match token with
            | Space -> @"\s"
            | Newline -> @"\n"
            | LineComment lexeme -> asPattern (PartialLineComment lexeme) + asPattern Newline
            | PartialLineComment _ -> @"//(?:(?!\n).)*"
            | BlockComment lexeme -> asPattern (PartialBlockComment lexeme) + asPattern AsteriskSlash
            | PartialBlockComment _ -> @"\/\*(?:(?!\*\/).|\n)*"
            // Atomic lexical elements
            | Letter _ -> @"[a-zA-Z]"
            | Nonzero _ -> @"[1-9]"
            | Digit _ -> @"[0-9]"
            | IntegerLiteral lexeme -> @"((" + asPattern (Nonzero lexeme) + asPattern (Digit lexeme) + "*)|0)"
            | Fraction lexeme -> @"((\." + asPattern (Digit lexeme) + "*" + asPattern (Nonzero lexeme) + ")|\.0)"
            | PartialFloat lexeme ->
                @"((" + asPattern (IntegerLiteral lexeme) + @")(\.)(" + asPattern (Digit lexeme) + @"*" + @")(e(\+|-)?)?)"
            | FloatLiteral lexeme ->
                @"((" + asPattern (IntegerLiteral lexeme) + @")(" + asPattern (Fraction lexeme) + @")(e(\+|-)?(" + asPattern (IntegerLiteral lexeme)
                + @"))?)"
            | Alphanum lexeme -> @"(" + asPattern (Letter lexeme) + @"|" + asPattern (Digit lexeme) + @"|_)"
            | Id lexeme -> @"(" + asPattern (Letter lexeme) + @"(" + asPattern (Alphanum lexeme) + @")*)"
            // Operators (single character)
            | Equal -> @"="
            | Plus -> @"\+"
            | Minus -> @"-"
            | Asterisk -> @"\*"
            | Lt -> @"<"
            | Gt -> @">"
            | Colon -> @":"
            | Slash -> @"/"
            | SemiColon -> @";"
            | Comma -> @","
            | Period -> @"\."
            | OpenBracket -> @"\("
            | ClosedBracket -> @"\)"
            | OpenSquareBracket -> @"\["
            | ClosedSquareBracket -> @"\]"
            | OpenBrace -> @"\{"
            | ClosedBrace -> @"\}"
            // Operators (double character)
            | EqualEqual -> @"=="
            | LtEqual -> @"<="
            | GtEqual -> @">="
            | LtGt -> @"<>"
            | ColonColon -> @"::"
            | SlashSlash -> @"//"
            | SlashAsterisk -> @"/\*"
            | AsteriskSlash -> @"\*/"
            // Keywords
            | If -> @"if"
            | Then -> @"then"
            | Else -> @"else"
            | While -> @"while"
            | Class -> @"class"
            | Integer -> @"integer"
            | Float -> @"float"
            | Do -> @"do"
            | End -> @"end"
            | Public -> @"public"
            | Private -> @"private"
            | Or -> @"or"
            | And -> @"and"
            | Not -> @"not"
            | Read -> @"read"
            | Write -> @"write"
            | Return -> @"return"
            | Main -> @"main"
            | Inherits -> @"inherits"
            | Local -> @"local"
            | Void -> @"void"
            | Invalid _ -> "[\s\S]*"
            | A -> @"A"
            | B -> @"B"
            | C -> @"C"
            | D -> @"D"

        @"\A" + asPattern x + @"\z"

[<RequireQualifiedAccess>]
module Lexer =
    Regex.CacheSize = 70 |> ignore

    let precedenceHighToLowTokens (lexeme: string) =
        [ Newline
          Space
          LineComment lexeme
          PartialLineComment lexeme
          BlockComment lexeme
          PartialBlockComment lexeme
          Equal
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
          Void
          Id lexeme
          FloatLiteral lexeme
          IntegerLiteral lexeme
          Letter lexeme
          Nonzero lexeme
          Digit lexeme
          Fraction lexeme
          PartialFloat lexeme
          Alphanum lexeme
          A
          B
          C
          D
          Invalid lexeme ]

    let isMatch (token: TokenType) (lexeme: string) = Regex.IsMatch(lexeme, token.pattern, RegexOptions.ExplicitCapture)

    let tryMatch (lexeme: string) (tokenType: TokenType) =
        match (isMatch tokenType lexeme) with
        | true -> Some tokenType
        | false -> None

    let matched (lexeme: string): TokenType list =
        //    let tokenUnionCaseNames = Utils.unionCaseNames<TokenType>
        //    let precedenceItemNames = precedenceHighToLowTokens lexeme |> List.map Utils.unionCaseName |> Set.ofList
        //    let missingTokensInPrecedenceList = Set.difference (tokenUnionCaseNames)  precedenceItemNames

        //    if Set.count missingTokensInPrecedenceList <> 0 then
        //        failwith ("Missing tokens in precedence list: " + String.concat ", " missingTokensInPrecedenceList)

        precedenceHighToLowTokens lexeme
        |> List.map (tryMatch lexeme)
        |> List.choose id

    let inline tokenized (lexeme: ^T) =
        List.head (matched (show lexeme) @ [ Invalid(show lexeme) ])

    let makeTokenStream (text: string) =
        let withoutIgnoreTokenHead (tokens: Token list) =
            match tokens with
            | head :: tail when head.tokenType.isIgnore = true -> tail
            | _ -> tokens

        let folder (tokensFound: Token list, location: Location) (item: char) =
            let tokenizedItem = tokenized item

            let lineOffset =
                if tokenizedItem = Newline then 1 else 0

            let columnOffset =
                if tokenizedItem = Newline then 0 else 1

            let location =
                { line = location.line + lineOffset
                  column = columnOffset * location.column + columnOffset }

            match tokensFound with
            | token :: rest ->
                let longerLexeme = token.lexeme + show item
                let longerToken = tokenized longerLexeme
                match longerToken with
                | _ when longerToken.isValid ->
                    ({ tokenType = longerToken
                       location =
                           { line = token.location.line
                             column = token.location.column } }
                     :: rest, location)
                | _ when token.tokenType.isIgnore ->
                    ({ tokenType = tokenizedItem
                       location =
                           { line = location.line
                             column = location.column } }
                     :: rest, location)
                | _ ->
                    ({ tokenType = tokenizedItem
                       location =
                           { line = location.line
                             column = location.column } }
                     :: tokensFound, location)
            | [] ->
                ({ tokenType = tokenizedItem
                   location =
                       { line = location.line
                         column = location.column } }
                 :: tokensFound, location)

        List.fold folder
            ([],
             { line = 1
               column = 1 }) (text |> List.ofSeq)
        |> fst
        |> withoutIgnoreTokenHead
        |> List.rev

    let tokenize (input: InputType) =
        match input with
        | FilePath path -> makeTokenStream (Utils.read path)
        | Text text -> makeTokenStream text

    let sanitizeTokens (tokens: Token list) =
        let sanitizeTokenFilter (token: Token) =
            match token.tokenType with
            | LineComment _
            | BlockComment _ -> false
            | tokenType when tokenType.isFinal -> true
            | _ -> false

        List.filter sanitizeTokenFilter tokens

    let drawTokens (tokens: Token list) =
        tokens
        |> List.map (fun token -> show token)
        |> String.concat "\n"

    let drawValidTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token -> token.tokenType.isValid)
        |> List.map (fun token -> show token)
        |> String.concat "\n"

    let drawInvalidTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token -> token.tokenType.isInvalid)
        |> List.map (fun token -> show token)
        |> String.concat "\n"

    let drawSanitizedTokens (tokens: Token list) =
        tokens
        |> sanitizeTokens
        |> List.map (fun token -> show token)
        |> String.concat "\n"

    let display (tokens: Token list): string =
        let mutable str = ""

        let mutable currentLine = tokens.[0].location.line

        for token in tokens do
            let line = token.location.line

            let outcomeAsString = show token

            if line = currentLine then
                str <- if str = "" then outcomeAsString else str + " " + outcomeAsString
            else
                currentLine <- line
                str <- str + "\n" + outcomeAsString
        str

    let writeTokens (tokens: Token list) (path: string) =
        Utils.write (display tokens) path

    let writeErrors (tokens: Token list) (path: string) =
        Utils.write
            (tokens
             |> List.filter (fun token -> token.tokenType.isPartial)
             |> List.map show
             |> List.fold (fun state e ->
                 if state = "" then e else state + "\n" + e) "") path
