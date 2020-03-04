module Moon.Grammar

open Moon.Language
open Moon.Extensions
open Moon.Lexer

type FirstType =
    | Terminal of string
    | Epsilon
    override m.ToString() =
        match m with
        | Terminal token -> token.ToString()
        | Epsilon -> "ε"
    static member from followType =
        match followType with
        | FollowType.Terminal terminal -> FirstType.Terminal terminal
        | DollarSign -> failwith "ABORT: Tried to FirstType.from DollarSign as followType!"

and FollowType =
    | Terminal of string
    | DollarSign
    override m.ToString() =
        match m with
        | Terminal token -> token.ToString()
        | DollarSign -> "$"
    static member from firstType =
        match firstType with
        | FirstType.Terminal terminal -> FollowType.Terminal terminal
        | Epsilon -> failwith "ABORT: Tried to FollowType.from Epsilon as firstType!"

type GrammarError =
    | CollisionsInTable of string list
    | InvalidVariable of string
    | InvalidSymbol of string

type GrammarSymbol =
    | Variable of Language.Variable
    | Terminal of Lexer.TokenType
    | SemanticAction of AST.NodeType
    | Epsilon
    override m.ToString() =
        match m with
        | Variable v -> v.ToString().ToUpper()
        | Terminal t -> t.ToString().ToLower()
        | SemanticAction a -> a.ToString()
        | Epsilon -> "eps"
    static member fromString (text: string): GrammarSymbol =
        let variableMaybe =
            match (Language.Variable.fromString text) with
            | Ok variable -> Some variable
            | Error _ -> None

        let terminalMaybe =
            match (Lexer.TokenType.fromString text) with
            | Ok terminal -> Some terminal
            | Error _ -> None

        let actionMaybe =
            match (AST.NodeType.fromString text) with
            | Ok action -> Some action
            | Error _ -> None

        if (Option.isSome variableMaybe) then
            let variable = variableMaybe |> Option.get
            (Variable variable)
        else if (Option.isSome terminalMaybe) then
            let terminal = terminalMaybe |> Option.get
            (Terminal terminal)
        else if (Option.isSome actionMaybe) then
            let action = actionMaybe |> Option.get
            (SemanticAction action)
        else
            Epsilon
    member m.isVariable =
        match m with
        | Variable _ -> true
        | _ -> false

type Production =
    { lhs: Language.Variable
      rhs: GrammarSymbol list }
    override m.ToString() =
        m.lhs.ToString().ToUpper() + " -> " + (String.concat " " (Seq.map (fun e -> e.ToString()) m.rhs))

type FirstSet = Set<FirstType>

type FollowSet = Set<FollowType>

type ParserTable = Map<Language.Variable * FollowType, int>

[<StructuredFormatDisplay("{display}")>]
type ContextFreeGrammar =
    { variables: Set<Language.Variable>
      terminals: Set<Lexer.TokenType>
      start: Language.Variable
      productions: Production list }
    member self.display =
        String.concat "\n" (List.map (fun production -> string production) self.productions)
    static member from (text: string): Result<ContextFreeGrammar, GrammarError> =
        let mutable variables: Set<Language.Variable> = Set.empty
        let mutable terminals: Set<Lexer.TokenType> = Set.empty
        let mutable semanticActions: Set<AST.NodeType> = Set.empty
        let mutable productions: Production list = List.empty
        let mutable errors: GrammarError list = List.empty

        for line in (text.Split("\n") |> Array.filter (fun e -> e <> "")) do
            let words: string list = List.ofSeq (line.Split(" ")) |> List.filter (fun e -> e <> "")

            let lhs =
                match Variable.fromString (words.[0]) with
                | Ok lhs ->
                    variables <- Set.union variables (Set.ofList [ lhs ])
                    Ok lhs
                | Error _ -> Error(InvalidVariable words.[0])

            let mutable rhs: GrammarSymbol list = List.empty

            for symbol in (Seq.safeSkip 2 words) do
                let variableMaybe =
                    match (Language.Variable.fromString symbol) with
                    | Ok variable -> Some variable
                    | Error _ -> None

                let terminalMaybe =
                    match (Lexer.TokenType.fromString symbol) with
                    | Ok terminal -> Some terminal
                    | Error _ -> None

                let actionMaybe =
                    match (AST.NodeType.fromString symbol) with
                    | Ok action -> Some action
                    | Error _ -> None

                if symbol = "." && Seq.length words = 3 then
                    rhs <- rhs @ [ Epsilon ]
                else if Option.isSome variableMaybe then
                    let variable = variableMaybe |> Option.get
                    variables <- Set.union variables (Set.ofList [ variable ])
                    rhs <- rhs @ [ Variable variable ]
                else if Option.isSome terminalMaybe then
                    let terminal = terminalMaybe |> Option.get
                    terminals <- Set.union terminals (Set.ofList [ terminal ])
                    rhs <- rhs @ [ Terminal terminal ]
                else if Option.isSome actionMaybe then
                    let action = actionMaybe |> Option.get
                    semanticActions <- Set.union semanticActions (Set.ofList [ action ])
                    rhs <- rhs @ [ SemanticAction action ]
                else if symbol <> "." then
                    errors <- errors @ [ InvalidSymbol("Invalid symbol: " + symbol) ]
                
            match lhs with
            | Ok value ->
                productions <-
                    productions @ [ { lhs = value
                                      rhs = rhs } ]
            | Error e ->
                errors <- errors @ [ e ]

        Ok
            ({ variables = variables
               terminals = terminals
               start = productions.[0].lhs
               productions = productions })


let firstFromRhs (first: Map<Language.Variable, FirstSet>) (rhs: GrammarSymbol list): FirstSet =
    let mutable set: FirstSet = Set.empty
    let mutable addEpsilon = true

    let mutable idx = 0
    let mutable keepIterating = true
    while keepIterating && idx < List.length rhs do
        let symbol = rhs.[idx]
        match symbol with
        | Terminal terminal ->
            addEpsilon <- false
            set <- set.Add(FirstType.Terminal terminal.case)
            keepIterating <- false
        | Variable variable ->
            let firstSetToUnion = first.[variable]
            if firstSetToUnion.Contains(FirstType.Epsilon) then
                let mutable withoutEpsilon = firstSetToUnion
                withoutEpsilon <- withoutEpsilon.Remove(FirstType.Epsilon)
                set <- Set.union set withoutEpsilon
            else
                addEpsilon <- false
                set <- Set.union set firstSetToUnion
                keepIterating <- false
        | Epsilon ->
            addEpsilon <- false
            set <- set.Add(FirstType.Epsilon)
            keepIterating <- false
        | SemanticAction _ -> ()
        idx <- idx + 1

    if addEpsilon then set <- set.Add(FirstType.Epsilon)

    set

let makeFirstSets (cfg: ContextFreeGrammar) =
    let mutable firstSets: Map<Language.Variable, FirstSet> = Map.empty
    
    for variable in cfg.variables do
        firstSets <- Map.add variable Set.empty firstSets

    let mutable modified = true

    while modified do
        modified <- false
        for production in cfg.productions do
            let mutable set = firstFromRhs firstSets production.rhs
            let variable = production.lhs
            let firstSet = firstSets.[variable]
            if not (firstSets = firstSets.Add(variable, Set.union firstSet set)) then
                firstSets <- firstSets.Add(variable, Set.union firstSet set)
                modified <- true

    firstSets

let makeFollowSets (cfg: ContextFreeGrammar) (firstSets: Map<Language.Variable, FirstSet>) =
    let mutable followSets: Map<Language.Variable, FollowSet> = Map.empty

    for variable in cfg.variables do
        if variable = cfg.start then followSets <- followSets.Add(variable, Set.ofList [ FollowType.DollarSign ])
        else followSets <- followSets.Add(variable, Set.empty)

    let mutable modified = true

    while modified = true do
        modified <- false
        for production in cfg.productions do
            for (idx, symbol) in List.indexed production.rhs do
                match symbol with
                | Variable variable ->
                    let firstAfterVariable = firstFromRhs firstSets production.rhs.[idx+1..]

                    let set2: Set<FollowType> =
                        firstAfterVariable
                        |> Set.filter (fun symbol -> symbol <> FirstType.Epsilon)
                        |> Set.map FollowType.from

                    let set3: FollowSet =
                        if firstAfterVariable.Contains(FirstType.Epsilon) then followSets.[production.lhs]
                        else Set.empty

                    let set = Set.union set2 set3

                    if not (followSets = followSets.Add(variable, Set.union followSets.[variable] set)) then
                        followSets <- followSets.Add(variable, Set.union followSets.[variable] set)
                        modified <- true
                | Terminal _ -> ()
                | SemanticAction _ -> ()
                | Epsilon -> ()

    followSets

let addEntry (cfg: ContextFreeGrammar) (table: ParserTable byref) (variable: Language.Variable) (terminal: FollowType) (productionNumber: int) =
    let oldValue = table.TryFind(variable, terminal)
    table <- table.Add((variable, terminal), productionNumber)
    match oldValue with
    | Some oldProductionNumber ->
        Error
            (sprintf "Collision at (%s, %s) between:\nOld: %s\nNew: %s" (variable.ToString()) (terminal.ToString())
                 (cfg.productions.[oldProductionNumber].ToString()) (cfg.productions.[productionNumber].ToString()))
    | None -> Ok(())

let makeTable (cfg: ContextFreeGrammar) (firstSets: Map<Language.Variable, FirstSet>) (followSets: Map<Language.Variable, FollowSet>): Result<ParserTable, GrammarError> =
    let mutable table: ParserTable = Map.empty
    let mutable collisions = List.empty

    for (productionNumber, production) in List.indexed cfg.productions do
        if (List.isEmpty production.rhs) then
            assert not (List.isEmpty production.rhs)

        let set = firstFromRhs firstSets production.rhs

        if set.Contains(FirstType.Epsilon) then
            for terminal in followSets.[production.lhs] do
                match addEntry cfg &table production.lhs terminal productionNumber with
                | Ok _ -> ()
                | Error collision -> collisions <- collisions @ [ collision ]

        for symbol in set do
            match symbol with
            | FirstType.Terminal terminal ->
                match addEntry cfg &table production.lhs (FollowType.Terminal terminal) productionNumber with
                | Ok _ -> ()
                | Error collision -> collisions <- collisions @ [ collision ]
            | FirstType.Epsilon -> ()


    if collisions.IsEmpty then (Ok table)
    else (Error(CollisionsInTable(collisions)))
    
let displayParseTable (cfg: ContextFreeGrammar) (table: Result<ParserTable, GrammarError>) =
    let mutable tableWithProductions: Map<Language.Variable * FollowType, Production> = Map.empty
    match table with
    | Ok parserTable ->
        for KeyValue((variable, terminal), n) in parserTable do
            tableWithProductions <- Map.add (variable, terminal) (cfg.productions.[n]) tableWithProductions
    | _ -> ignore ()
    
    let okFromResult (result: Result<'A, 'E>) =
        match result with
        | Ok success -> success
        | Error _ -> failwith "okFromResult: Tried to extract Ok value from Error"
        
    let mutable items = List.empty
    for KeyValue((variable, terminal), production) in tableWithProductions do
        items <- items @ [ (((variable |> string).ToUpper(), ((terminal |> string).ToLower() |> TokenType.fromString |> okFromResult |> string).ToLower()), production) ]
    
    let comparer (item1: ((string * string) * Production)) (item2: ((string * string) * Production)) =
        let v1 = (item1 |> fst |> fst)
        let t1 = (item1 |> fst |> snd)
        let v2 = (item2 |> fst |> fst)
        let t2 = (item2 |> fst |> snd)
        
        if v1 < v2 then
            -1
        else if v1 > v2 then
            1
        else
            if t1 < t2 then
                -1
            else if t1 > t2 then
                1
            else
                0
        
    items <- List.sortWith comparer items
    
    let mutable str = ""
    for ((variable, terminal), production) in items do
        str <- str + "(" + variable + ", " + terminal + "): " + string production + "\n"
    str