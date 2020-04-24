namespace Moon

type ParserSymbol =
    | Variable of NonTerminal
    | Terminal of TokenType
    | SemanticAction of SyntaxKind
    | DollarSign
    static member fromGrammarSymbol (gs: GrammarSymbol) =
        match gs with
        | GrammarSymbol.Terminal t ->
            ParserSymbol.Terminal t
        | GrammarSymbol.Variable v ->
            ParserSymbol.Variable v
        | GrammarSymbol.SemanticAction a ->
            ParserSymbol.SemanticAction a
        | Epsilon -> failwith "ABORT: Tried to ParserSymbol.fromGrammarSymbol Epsilon as GrammarSymbol!"

[<StructuredFormatDisplay("{show}")>]
type SyntaxError =
    | WrongTerminal of Token * TokenType * Location option
    | WrongVariable of string
    | NotInTableButInFollow of Token * NonTerminal * bool
    | NotInTableNorInFollow of Token * NonTerminal * Location option
    | TokenAfterMain of Token

    member x.uniqueId =
        match x with
        | WrongTerminal (token, _, _) -> token
        | WrongVariable s -> { tokenType = TokenType.Id s; location = { line = 0; column = 0 } }
        | NotInTableButInFollow (token, _, _) -> token
        | NotInTableNorInFollow (token, _, _) -> token
        | TokenAfterMain token -> token

    member x.show =
        let displayLocationMaybe (locationMaybe: Location option) =
            match locationMaybe with
            | Some location -> show location
            | None -> "line=NA, column=NA"
        match x with
        | WrongTerminal(token, tokenType, locationMaybe) ->
            "WrongTerminal: token='" + show token + "', tokenType='" + show tokenType + "', " + show locationMaybe
        | WrongVariable variable -> "WrongVariable: variable='" + show variable + "'"
        | NotInTableButInFollow(token, variable, isDollarSign) ->
            "NotInTableButInFollow: token='" + show token + "', variable='" + show variable + "', isNotDollarSign=" + show isDollarSign
        | NotInTableNorInFollow(token, variable, locationMaybe) ->
            "NotInTableNorInFollow: token='" + show token + "', variable='" + show variable + "', " + displayLocationMaybe locationMaybe
        | TokenAfterMain token -> "TokenAfterMain: token='" + show token + "'"

type OptionalProduction = Production option

type DerivationTable = (GrammarSymbol list * OptionalProduction) list

type ParserOutput = Result<IndexedAst * DerivationTable * SyntaxError list, ASTError>

type Parser =
    { grammar: Grammar
      table: ParseTable
      firstSets: Map<NonTerminal, FirstSet>
      followSets: Map<NonTerminal, FollowSet> }

    static member fromGrammar (grammar: Grammar): Result<Parser, GrammarError> =
        let firstSets = Grammar.makeFirstSets grammar
        let followSets = Grammar.makeFollowSets grammar firstSets
        let table = Grammar.makeParseTable grammar firstSets followSets
        match table with
        | Ok parserTable ->
            Ok
                { grammar = grammar
                  table = parserTable
                  firstSets = firstSets
                  followSets = followSets }
        | Error grammarError ->
            Error grammarError

    member x.start = x.grammar.start

    member x.get (variable: NonTerminal) (input: FollowType): Production option =
        Map.tryFind (variable, input) x.table |> Option.map (fun e -> x.grammar.productions.[e])

    member x.firstSetsAsString: string =
        let mutable str = ""
        for KeyValue(lhs, rhs) in x.firstSets do
            str <- str + show lhs + ": {"
            for symbol in rhs do
                str <- str + " " + show symbol
            str <- " }\n"
        str


    member x.followSetsAsString: string =
        let mutable str = ""
        for KeyValue(lhs, rhs) in x.followSets do
            str <- str + show lhs + ": {"
            for symbol in rhs do
                str <- str + " " + show symbol
            str <- " }\n"
        str

    static member top (stack: ParserSymbol list) = List.last stack

    static member pop (stack: ParserSymbol list) =
        if List.length stack > 0
        then List.truncate (List.length stack - 1) stack
        else stack

    static member nextToken (idx: int) (tokens: Token list) =
        let nextIdx = idx + 1
        if nextIdx < List.length tokens then
            let token = tokens.[nextIdx].tokenType
            (nextIdx, Some token)
        else
            (nextIdx, None)

    member x.parse (tokens: Token list) =
        let mutable parserSymbols = List.map (FollowType.Terminal) (tokens |> List.map (fun e -> e.tokenType.case))
        parserSymbols <- parserSymbols @ [ FollowType.DollarSign ]

        let mutable tokenIter = Iter.make (tokens)
        let mutable tokenTypeIter = Iter.make (parserSymbols)
        let mutable token = tokenIter.next() |> Option.get
        let mutable tokenType = tokenTypeIter.next() |> Option.get
        let mutable lastToken = None

        let mutable stack =
            [ ParserSymbol.DollarSign
              ParserSymbol.Variable x.grammar.start ]

        let mutable ast = IndexedAst.make
        let mutable semanticStack = List.empty
        let mutable tokenStack: Token list = List.empty

        let mutable syntacticErrors: SyntaxError list = List.empty

        let mutable derivation: GrammarSymbol list = [ GrammarSymbol.Variable x.grammar.start ]
        let mutable derivationTable: DerivationTable = List.empty

        let mutable shouldContinue = true

        while shouldContinue do
            match List.tryLast stack with
            | Some symbol ->
                match symbol with
                | Terminal terminal ->
                    if tokenType = FollowType.Terminal terminal.case then
                        List.pop &stack |> ignore
                        tokenType <- tokenTypeIter.next() |> Option.get
                        if tokenType <> FollowType.DollarSign then
                            lastToken <- Some token
                            token <- tokenIter.next() |> Option.get
                        else
                            shouldContinue <- true
                    else
                        let (startToken, errorTerminal) = (token, terminal)
                        while tokenType <> FollowType.Terminal terminal.case && Option.isSome (tokenTypeIter.peek()) do
                            tokenType <- tokenTypeIter.next() |> Option.get
                            if tokenType = FollowType.DollarSign then
                                syntacticErrors <- syntacticErrors @ [ WrongTerminal(startToken, errorTerminal, None) ]
                                shouldContinue <- false
                            if shouldContinue then
                                lastToken <- Some token
                                token <- tokenIter.next() |> Option.get

                        syntacticErrors <- syntacticErrors @ [ WrongTerminal(startToken, errorTerminal, Some(token.location)) ]
                | Variable variable ->
                    let productionMaybe = x.get variable tokenType

                    match productionMaybe with
                    | Some production ->
                        List.pop &stack |> ignore

                        for symbol in List.rev production.rhs do
                            if symbol <> GrammarSymbol.Epsilon then stack <- stack @ [ ParserSymbol.fromGrammarSymbol symbol ]

                        derivationTable <- derivationTable @ [ (derivation, Some production) ]

                        match derivation
                              |> List.indexed
                              |> List.tryFind (fun (_, e: GrammarSymbol) -> e.isVariable) with
                        | Some(position, var) ->
                            if var <> GrammarSymbol.Variable(production.lhs) then
                                shouldContinue <- false
                                syntacticErrors <- syntacticErrors @ [ WrongVariable "PANIC: Variable found is not the same as production.lhs!" ]

                            List.remove position &derivation

                            for symbol in List.rev production.rhs do
                                match symbol with
                                | GrammarSymbol.Variable variable ->
                                    List.insert (GrammarSymbol.Variable variable) position &derivation
                                | GrammarSymbol.Terminal terminal ->
                                    List.insert (GrammarSymbol.Terminal terminal) position &derivation
                                | _ -> ()
                        | None ->
                            shouldContinue <- false
                            syntacticErrors <- syntacticErrors @ [ WrongVariable "PANIC: No variable found in derivation" ]
                    | None ->
                        if tokenType = FollowType.DollarSign then
                            syntacticErrors <- syntacticErrors @ [ NotInTableButInFollow(token, variable, false) ]
                            shouldContinue <- false
                        else if Set.contains tokenType x.followSets.[variable] then
                            syntacticErrors <- syntacticErrors @ [ NotInTableButInFollow(token, variable, true) ]
                            tokenType <- tokenTypeIter.next() |> Option.get
                            lastToken <- Some token
                            token <- tokenIter.next() |> Option.get
                        else
                            let (startToken, errorVariable) = (token, variable)
                            syntacticErrors <- syntacticErrors @ [ NotInTableNorInFollow(startToken, errorVariable, Some(token.location)) ]
                            let mutable shouldContinueHere = true
                            while shouldContinueHere && x.get variable tokenType |> Option.isNone do
                                tokenType <- tokenTypeIter.next() |> Option.get
                                if tokenType = FollowType.DollarSign then
                                    syntacticErrors <- syntacticErrors @ [ NotInTableNorInFollow(token, errorVariable, None) ]
                                    shouldContinueHere <- false
                                    shouldContinue <- false
                                if shouldContinueHere then
                                    lastToken <- Some token
                                    token <- tokenIter.next() |> Option.get
                            if shouldContinue then
                                syntacticErrors <- syntacticErrors @ [ NotInTableNorInFollow(startToken, errorVariable, Some(token.location)) ]
                | SemanticAction semanticAction ->
                    match semanticAction with
                    | Data ->
                        tokenStack <- tokenStack @ [ Option.get lastToken ]
                    | EndProgram ->
                        List.pop &stack |> ignore
                        if tokenType <> FollowType.DollarSign then tokenIter.items <- tokenIter.items @ [ token ] // TODO: Hack af
                        shouldContinue <- false
                    | _ ->
                        match ast.makeNode (&semanticStack, &tokenStack, semanticAction) with
                        | Ok _ -> ()
                        | Error failure -> ast.errors <- ast.errors @ [ failure ]
                    List.pop &stack |> ignore
                | DollarSign ->
                    shouldContinue <- false
            | None ->
                shouldContinue <- false

        for newToken in tokenIter.rest() do
            syntacticErrors <- syntacticErrors @ [ TokenAfterMain newToken ]

        ast.index <- List.pop &semanticStack

        derivationTable <- derivationTable @ [ (derivation, None) ]

        syntacticErrors <- List.distinctBy (fun (e: SyntaxError) -> e.uniqueId) syntacticErrors

        Ok(ast, derivationTable, syntacticErrors)

    member x.sanitizeTokensAndParse (tokens: Token list) =
        let sanitizedTokens = Lexer.sanitizeTokens tokens
        x.parse (sanitizedTokens)

[<RequireQualifiedAccess>]
module Parser =
    open FSharpPlus

    let drawFirstSets (firstSets: Map<NonTerminal, FirstSet>): string =
        let mutable str = ""
        for KeyValue(lhs, rhs) in firstSets do
            str <- str + show lhs + ": {"
            for symbol in rhs do
                str <- str + " " + show symbol
            str <- str + " }\n"
        str

    let drawFollowSets (followSets: Map<NonTerminal, FollowSet>): string =
        let mutable str = ""
        for KeyValue(lhs, rhs) in followSets do
            str <- str + show lhs + ": {"
            for symbol in rhs do
                str <- str + " " + show symbol
            str <- str + " }\n"
        str

    let drawParseTable (cfg: Grammar) (table: Result<ParseTable, GrammarError>) =
        let mutable tableWithProductions: Map<NonTerminal * FollowType, Production> = Map.empty
        match table with
        | Ok parserTable ->
            for KeyValue((variable, terminal), n) in parserTable do
                tableWithProductions <- Map.add (variable, terminal) (cfg.productions.[n]) tableWithProductions
        | _ -> ignore()

        let okFromResult (result: Result<'A, 'E>) =
            match result with
            | Ok success -> success
            | Error _ -> failwith "okFromResult: Tried to extract Ok value from Error"

        let mutable items = List.empty
        for KeyValue((variable, terminal), production) in tableWithProductions do
            items <-
                items
                @ [ ((String.toUpper << show) variable,
                     (String.toLower << show << okFromResult << TokenType.fromString << String.toLower << show) terminal), production ]

        let comparer (item1: (string * string) * Production) (item2: (string * string) * Production) =
            let v1 =
                item1
                |> fst
                |> fst

            let t1 =
                item1
                |> fst
                |> snd

            let v2 =
                item2
                |> fst
                |> fst

            let t2 =
                item2
                |> fst
                |> snd

            if v1 < v2 then -1
            elif v1 > v2 then 1
            elif t1 < t2 then -1
            elif t1 > t2 then 1
            else 0

        items <- List.sortWith comparer items

        let mutable str = ""
        for ((variable, terminal), production) in items do
            str <- str + "(" + variable + ", " + terminal + "): " + show production + "\n"
        str

    let drawDerivationTable (derivationTable: DerivationTable) =
        let mutable str = ""
        for (grammarSymbols, productionMaybe) in derivationTable do
            let production =
                if Option.isSome productionMaybe then show (productionMaybe |> Option.get) else ""

            let symbols = List.map (fun e -> show e) grammarSymbols |> String.concat " "
            str <- str + "[" + production + "]: \n\t" + symbols + "\n\n"
        str

    let drawSyntaxErrors (syntaxErrors: SyntaxError list) =
        let mutable str = ""
        for syntaxError in syntaxErrors do
            str <- str + show syntaxError + "\n"
        str

    let drawIndexedAst (ast: IndexedAst) =
        let mutable str = ""
        for node in ast.nodes do
            str <- str + show node + "\n"
        str

    let drawAstErrors (errors: ASTError list) =
        String.concat "\n" (List.map show errors)
