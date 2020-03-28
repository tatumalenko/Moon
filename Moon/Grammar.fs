namespace Moon

open FSharpPlus

[<StructuredFormatDisplay("{show}")>]
type FirstType =
    | Terminal of string
    | Epsilon

    member x.show =
        match x with
        | Terminal token -> show token
        | Epsilon -> "ε"

    static member from followType =
        match followType with
        | FollowType.Terminal terminal -> FirstType.Terminal terminal
        | DollarSign -> failwith "ABORT: Tried to FirstType.from DollarSign as followType!"

and [<StructuredFormatDisplay("{show}")>] FollowType =
    | Terminal of string
    | DollarSign

    member x.show =
        match x with
        | Terminal token -> show token
        | DollarSign -> "$"

    static member from firstType =
        match firstType with
        | FirstType.Terminal terminal -> FollowType.Terminal terminal
        | Epsilon -> failwith "ABORT: Tried to FollowType.from Epsilon as firstType!"

type GrammarError =
    | CollisionsInTable of string list
    | InvalidVariable of string
    | InvalidSymbol of string

[<StructuredFormatDisplay("{show}")>]
type GrammarSymbol =
    | Variable of NonTerminal
    | Terminal of TokenType
    | SemanticAction of SyntaxKind
    | Epsilon

    member x.show =
        match x with
        | Variable v -> (String.toUpper << show) v
        | Terminal t -> (String.toLower << show) t
        | SemanticAction a -> show a
        | Epsilon -> "eps"

    static member fromString (text: string): GrammarSymbol =
        let variableMaybe =
            match (NonTerminal.fromString text) with
            | Ok variable -> Some variable
            | Error _ -> None

        let terminalMaybe =
            match (TokenType.fromString text) with
            | Ok terminal -> Some terminal
            | Error _ -> None

        let actionMaybe =
            match (SyntaxKind.fromString text) with
            | Ok action -> Some action
            | Error _ -> None

        if (Option.isSome variableMaybe) then
            let variable = variableMaybe |> Option.get
            (Variable variable)
        elif (Option.isSome terminalMaybe) then
            let terminal = terminalMaybe |> Option.get
            (Terminal terminal)
        elif (Option.isSome actionMaybe) then
            let action = actionMaybe |> Option.get
            (SemanticAction action)
        else
            Epsilon

    member x.isVariable =
        match x with
        | Variable _ -> true
        | _ -> false

[<StructuredFormatDisplay("{show}")>]
type Production =
    { lhs: NonTerminal
      rhs: GrammarSymbol list }
    member x.show = (String.toUpper << show) x.lhs + " -> " + (String.concat " " (Seq.map (fun e -> show e) x.rhs))

type FirstSet = Set<FirstType>

type FollowSet = Set<FollowType>

type ParseTable = Map<NonTerminal * FollowType, int>

[<StructuredFormatDisplay("{show}")>]
type Grammar =
    { variables: Set<NonTerminal>
      terminals: Set<TokenType>
      start: NonTerminal
      productions: Production list }

    member x.show =
        String.concat "\n" (List.map (fun production -> show production) x.productions)

[<RequireQualifiedAccess>]
module Grammar =
    let from (text: string): Result<Grammar, GrammarError> =
        let mutable variables: Set<NonTerminal> = Set.empty
        let mutable terminals: Set<TokenType> = Set.empty
        let mutable semanticActions: Set<SyntaxKind> = Set.empty
        let mutable productions: Production list = List.empty
        let mutable errors: GrammarError list = List.empty

        for line in (String.split [ "\n" ] text |> Seq.filter (not << String.isBlankOrEmpty)) do
            let words: string list =
                String.split [ " " ] line
                |> Seq.filter (not << String.isBlankOrEmpty)
                |> List.ofSeq
                |> List.map (String.replace "\t" " ")

            let lhs =
                match NonTerminal.fromString (String.replace " " "" words.[0]) with
                | Ok lhs ->
                    variables <- Set.union variables (Set.ofList [ lhs ])
                    Ok lhs
                | Error _ -> Error(InvalidVariable words.[0])

            let mutable rhs: GrammarSymbol list = List.empty

            for symbol in (Seq.safeSkip 2 words) do
                let variableMaybe =
                    match (NonTerminal.fromString symbol) with
                    | Ok variable -> Some variable
                    | Error _ -> None

                let terminalMaybe =
                    match (TokenType.fromString symbol) with
                    | Ok terminal -> Some terminal
                    | Error _ -> None

                let actionMaybe =
                    match (SyntaxKind.fromString symbol) with
                    | Ok action -> Some action
                    | Error _ -> None

                if symbol = "EPSILON" && Seq.length words = 3 then
                    rhs <- rhs @ [ Epsilon ]
                elif Option.isSome variableMaybe then
                    let variable = variableMaybe |> Option.get
                    variables <- Set.union variables (Set.ofList [ variable ])
                    rhs <- rhs @ [ Variable variable ]
                elif Option.isSome terminalMaybe then
                    let terminal = terminalMaybe |> Option.get
                    terminals <- Set.union terminals (Set.ofList [ terminal ])
                    rhs <- rhs @ [ Terminal terminal ]
                elif Option.isSome actionMaybe then
                    let action = actionMaybe |> Option.get
                    semanticActions <- Set.union semanticActions (Set.ofList [ action ])
                    rhs <- rhs @ [ SemanticAction action ]
                elif symbol <> "EPSILON" then
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

    let firstFromRhs (first: Map<NonTerminal, FirstSet>) (rhs: GrammarSymbol list): FirstSet =
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

    let makeFirstSets (cfg: Grammar) =
        let mutable firstSets: Map<NonTerminal, FirstSet> = Map.empty

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

    let makeFollowSets (cfg: Grammar) (firstSets: Map<NonTerminal, FirstSet>) =
        let mutable followSets: Map<NonTerminal, FollowSet> = Map.empty

        for variable in cfg.variables do
            if variable = cfg.start
            then followSets <- followSets.Add(variable, Set.ofList [ FollowType.DollarSign ])
            else followSets <- followSets.Add(variable, Set.empty)

        let mutable modified = true

        while modified = true do
            modified <- false
            for production in cfg.productions do
                for (idx, symbol) in List.indexed production.rhs do
                    match symbol with
                    | Variable variable ->
                        let firstAfterVariable = firstFromRhs firstSets production.rhs.[idx + 1..]

                        let set2: Set<FollowType> =
                            firstAfterVariable
                            |> Set.filter (fun symbol -> symbol <> FirstType.Epsilon)
                            |> Set.map FollowType.from

                        let set3: FollowSet =
                            if firstAfterVariable.Contains(FirstType.Epsilon)
                            then followSets.[production.lhs]
                            else Set.empty

                        let set = Set.union set2 set3

                        if not (followSets = followSets.Add(variable, Set.union followSets.[variable] set)) then
                            followSets <- followSets.Add(variable, Set.union followSets.[variable] set)
                            modified <- true
                    | Terminal _ -> ()
                    | SemanticAction _ -> ()
                    | Epsilon -> ()

        followSets

    let addEntry (cfg: Grammar) (table: ParseTable byref) (variable: NonTerminal) (terminal: FollowType) (productionNumber: int) =
        let oldValue = table.TryFind(variable, terminal)
        table <- table.Add((variable, terminal), productionNumber)
        match oldValue with
        | Some oldProductionNumber ->
            Error
                (sprintf "Collision at (%s, %s) between:\nOld: %s\nNew: %s" (show variable) (show terminal)
                     (show cfg.productions.[oldProductionNumber]) (show cfg.productions.[productionNumber]))
        | None -> Ok(())

    let makeParseTable
        (cfg: Grammar)
        (firstSets: Map<NonTerminal, FirstSet>)
        (followSets: Map<NonTerminal, FollowSet>)
        : Result<ParseTable, GrammarError> =
        let mutable table: ParseTable = Map.empty
        let mutable collisions = List.empty

        for (productionNumber, production) in List.indexed cfg.productions do
            if (List.isEmpty production.rhs) then assert not (List.isEmpty production.rhs)

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


        if collisions.IsEmpty then (Ok table) else (Error(CollisionsInTable(collisions)))

    let displayParseTable (cfg: Grammar) (table: Result<ParseTable, GrammarError>) =
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
