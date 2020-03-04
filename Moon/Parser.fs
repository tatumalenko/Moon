module Moon.Parser

open Moon.Extensions
open Moon.Lexer
open Moon.AST
open Moon.Grammar
open Moon.Language

type ParserSymbol =
    | Variable of Language.Variable
    | Terminal of Lexer.TokenType
    | SemanticAction of AST.NodeType
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

[<StructuredFormatDisplay("{display}")>]
type SyntaxError =
    | WrongTerminal of Lexer.Token * Lexer.TokenType * Lexer.Location option
    | WrongVariable of string
    | NotInTableButInFollow of Lexer.Token * Language.Variable * bool
    | NotInTableNorInFollow of Lexer.Token * Language.Variable * Lexer.Location option
    | TokenAfterMain of Lexer.Token
    member self.display =
        let displayLocationMaybe (locationMaybe: Location option) =
            match locationMaybe with
            | Some location -> string location
            | None -> "line=NA, column=NA"
        match self with
        | WrongTerminal (token, tokenType, locationMaybe) -> "WrongTerminal: token='" + string token + "', tokenType='" + string tokenType + "', " + string locationMaybe 
        | WrongVariable variable -> "WrongVariable: variable='" + string variable + "'"
        | NotInTableButInFollow (token, variable, isDollarSign) -> "NotInTableButInFollow: token='" + string token + "', variable='" + string variable + "', isNotDollarSign=" + string isDollarSign
        | NotInTableNorInFollow (token, variable, locationMaybe) -> "NotInTableNorInFollow: token='" + string token + "', variable='" + string variable + "', " + displayLocationMaybe locationMaybe
        | TokenAfterMain token -> "TokenAfterMain: token='" + string token + "'"

type OptionalProduction = Production option
type DerivationTable = (GrammarSymbol list * OptionalProduction) list
type ParserOutput = Result<(AST * DerivationTable * SyntaxError list), ASTError>

type Parser =
    { grammar: ContextFreeGrammar
      table: ParserTable
      firstSets: Map<Language.Variable, FirstSet>
      followSets: Map<Language.Variable, FollowSet> }

    static member fromGrammar (grammar: ContextFreeGrammar): Result<Parser, GrammarError> =
        let firstSets = Grammar.makeFirstSets grammar
        let followSets = Grammar.makeFollowSets grammar firstSets
        let table = Grammar.makeTable grammar firstSets followSets
        match table with
        | Ok parserTable ->
            Ok
                { grammar = grammar
                  table = parserTable
                  firstSets = firstSets
                  followSets = followSets }
        | Error grammarError ->
            Error grammarError

    member m.start = m.grammar.start

    member m.get (variable: Variable) (input: FollowType): Production option =
        Map.tryFind (variable, input) m.table |> Option.map (fun e -> m.grammar.productions.[e])

    member m.firstSetsAsString: string =
        let mutable str = ""
        for KeyValue(lhs, rhs) in m.firstSets do
            str <- str + lhs.ToString() + ": {"
            for symbol in rhs do
                str <- str + " " + symbol.ToString()
            str <- " }\n"
        str

    member m.followSetsAsString: string =
        let mutable str = ""
        for KeyValue(lhs, rhs) in m.followSets do
            str <- str + lhs.ToString() + ": {"
            for symbol in rhs do
                str <- str + " " + symbol.ToString()
            str <- " }\n"
        str
        
    static member top (stack: ParserSymbol list) =
        List.last stack
        
    static member pop (stack: ParserSymbol list) =
        if List.length stack > 0 then
            List.truncate (List.length stack - 1) stack
        else stack
        
    static member nextToken (idx: int) (tokens: Lexer.Token list) =
        let nextIdx = idx + 1
        if nextIdx < List.length tokens then
            let token = tokens.[nextIdx].tokenType
            (nextIdx, Some token)
        else
            (nextIdx, None)
            
    member m.parse (tokens: Lexer.Token list) = 
        let mutable parserSymbols = List.map (FollowType.Terminal) (tokens |> List.map (fun e -> e.tokenType.case))
        parserSymbols <- parserSymbols @ [FollowType.DollarSign]
        
        let mutable tokenIter = Iter.make(tokens) 
        let mutable tokenTypeIter = Iter.make(parserSymbols)
        let mutable token = tokenIter.next() |> Option.get
        let mutable tokenType = tokenTypeIter.next() |> Option.get
        let mutable lastToken = None
        
        let mutable stack = [ ParserSymbol.DollarSign; ParserSymbol.Variable m.grammar.start ]
        
        let mutable ast = AST.make()
        let mutable semanticStack = List.empty
        let mutable tokenStack: Lexer.Token list = List.empty
        
        let mutable syntacticErrors: SyntaxError list = List.empty
        
        let mutable derivation: GrammarSymbol list = [ GrammarSymbol.Variable m.grammar.start ]
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
                        else shouldContinue <- false
                    else
                        let (startToken, errorTerminal) = (token, terminal)
                        while tokenType <> FollowType.Terminal terminal.case && Option.isSome (tokenTypeIter.peek()) do
                            tokenType <- tokenTypeIter.next() |> Option.get
                            if tokenType = FollowType.DollarSign then
                                syntacticErrors <- syntacticErrors @ [ WrongTerminal (startToken, errorTerminal, None) ]
                                shouldContinue <- false
                            if shouldContinue then
                                lastToken <- Some token
                                token <- tokenIter.next() |> Option.get
                        
                        syntacticErrors <- syntacticErrors @ [ WrongTerminal (startToken, errorTerminal, Some (token.location)) ]
                | Variable variable ->
                    let productionMaybe = m.get variable tokenType

                    match productionMaybe with
                    | Some production ->
                        List.pop &stack |> ignore
                        
                        for symbol in List.rev production.rhs do
                            if symbol <> GrammarSymbol.Epsilon then
                                stack <- stack @ [ ParserSymbol.fromGrammarSymbol symbol ]
                            
                        derivationTable <- derivationTable @ [ (derivation, Some production) ]
                        
                        match derivation
                            |> List.indexed
                            |> List.tryFind (fun (_, e: GrammarSymbol) -> e.isVariable) with
                        | Some (position, var) ->
                            if var <> GrammarSymbol.Variable (production.lhs) then
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
                            syntacticErrors <- syntacticErrors @ [ NotInTableButInFollow (token, variable, false) ]
                            shouldContinue <- false
                        else if Set.contains tokenType m.followSets.[variable] then
                            syntacticErrors <- syntacticErrors @ [ NotInTableButInFollow (token, variable, true) ]
                            tokenType <- tokenTypeIter.next() |> Option.get
                            lastToken <- Some token
                            token <- tokenIter.next() |> Option.get
                        else
                            let (startToken, errorVariable) = (token, variable)
                            syntacticErrors <- syntacticErrors @ [ NotInTableNorInFollow (startToken, errorVariable, Some (token.location)) ]
                            let mutable shouldContinueHere = true
                            while shouldContinueHere && m.get variable tokenType |> Option.isNone do
                                tokenType <- tokenTypeIter.next() |> Option.get
                                if tokenType = FollowType.DollarSign then
                                    syntacticErrors <- syntacticErrors @ [ NotInTableNorInFollow (token, errorVariable, None) ]
                                    shouldContinueHere <- false
                                    shouldContinue <- false
                                if shouldContinueHere then
                                    lastToken <- Some token
                                    token <- tokenIter.next() |> Option.get
                                    
                            if shouldContinue then
                                syntacticErrors <- syntacticErrors @ [ NotInTableNorInFollow (startToken, errorVariable, Some (token.location)) ]
                | SemanticAction semanticAction ->
                    match semanticAction with
                    | Data ->
                        tokenStack <- tokenStack @ [ Option.get lastToken ]
                    | EndProgram ->
                        List.pop &stack |> ignore
                        if tokenType <> FollowType.DollarSign then
                            tokenIter.items <- tokenIter.items @ [ token ] // TODO: Hack af
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
         
        ast.root <- List.pop &semanticStack
        
        derivationTable <- derivationTable @ [ (derivation, None) ]
        
        Ok (ast, derivationTable, syntacticErrors)

    member self.sanitizeTokensAndParse (tokens: Lexer.Token list) =
        let sanitizedTokens = Lexer.sanitizeTokens tokens
        self.parse(sanitizedTokens)

let firstSetsAsString (firstSets: Map<Language.Variable, FirstSet>): string =
    let mutable str = ""
    for KeyValue(lhs, rhs) in firstSets do
        str <- str + lhs.ToString() + ": {"
        for symbol in rhs do
            str <- str + " " + symbol.ToString()
        str <- str + " }\n"
    str

let followSetsAsString (followSets: Map<Language.Variable, FollowSet>): string =
    let mutable str = ""
    for KeyValue(lhs, rhs) in followSets do
        str <- str + lhs.ToString() + ": {"
        for symbol in rhs do
            str <- str + " " + symbol.ToString()
        str <- str + " }\n"
    str
    
let parserTableAsString (parserTable: ParserTable) =
    let mutable str = ""
    for KeyValue((variable, terminal), n) in parserTable do
        str <- str + "(" + variable.ToString() + ", " + terminal.ToString() + "): " + string n + "\n"
    str
    
let derivationTableAsString (derivationTable: DerivationTable) =
    let mutable str = ""
    for (grammarSymbols, productionMaybe) in derivationTable do
        let production = if Option.isSome productionMaybe then (productionMaybe |> Option.get).ToString() else ""
        let symbols = List.map (fun e -> e.ToString()) grammarSymbols |> String.concat " " 
        str <- str + "[" + production + "]: \n\t" + symbols + "\n\n"
    str
    
let syntaxErrorsAsString (syntaxErrors: SyntaxError list) =
    let mutable str = ""
    for syntaxError in syntaxErrors do
        str <- str  + syntaxError.ToString() + "\n"
    str
    
let astAsString (ast: AST) =
    let mutable str = ""
    for node in ast.nodes do
        str <- str  + node.ToString() + "\n"
    str