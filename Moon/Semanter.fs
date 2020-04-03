namespace Moon

[<StructuredFormatDisplay("{show}")>]
type SemanticError =
    // Symbol table phase -- creation
    | MultiplyDeclaredLocalVariable of Token * string
    | MultiplyDeclaredParameter of Token * string
    | MultiplyDeclaredFreeFunction of Token * string // Same name and type signatures (not overloaded)
    | MultiplyDeclaredMemberVariable of Token * string
    | MultiplyDeclaredMemberFunction of Token * string // Same name and type signatures (not overloaded)
    | MultiplyDeclaredClass of Token

    | UndefinedFreeFunction of Token
    | UndefinedMemberVariable of Token * SymbolType
    | UndefinedMemberFunction of Token * SymbolType

    // Semantic check phase -- binding and type checking
    | UndeclaredLocalVariable of Token * string
    | UndeclaredMemberVariable of Token * string
    | UndeclaredMemberFunction of Token * string
    | UndeclaredClass of Token

    | TypeMismatch of Token * SymbolType * SymbolType // Different types between lhs/rhs
    | TypeDimensionMismatch of Token * int * int // Different dimensions between decl/def

    | ArrayDimensionMismatch of Token * SymbolTable * SymbolType // Different dimensions between decl/def
    | ArrayDimensionNonPositive of Token * int * int // Declaration uses negative integer
    | ArrayIndexNonInteger of Token * int * SymbolType // Indexed expr uses non-integer
    | ArrayIndexNonPositive // Indexed expr uses negative integer

    | FunctionWithoutReturn of Token
    | FunctionReturnTypeMismatch of Token * SymbolType * SymbolType
    | FunctionParamTypeMismatch of Token * int * SymbolType * SymbolType
    | FunctionArityMismatch of Token * int * int
    | FunctionMainWithReturn of Token // ??

    | AddTypeMismatch of Token * SymbolType * SymbolType
    | MultTypeMismatch of Token * SymbolType * SymbolType
    | RelTypeMismatch of Token * SymbolType * SymbolType

    | AddTypeInvalid of Token * SymbolType * SymbolType
    | MultTypeInvalid of Token * SymbolType * SymbolType
    | RelTypeInvalid of Token * SymbolType * SymbolType

    | CircularInheritance of Token * string list
    | ShadowedMemberVariableInheritance of Token * string * string * string // Warning only
    | ShadowedMemberFunctionInheritance of Token * string * string * string // Warning only
    | OverloadedFreeFunction of Token * SymbolType * SymbolType // Warning only
    | OverloadedMemberFunction of Token * SymbolType * SymbolType // Warning only

    member inline x.isWarning =
        match x with
        | OverloadedFreeFunction _
        | OverloadedMemberFunction _
        | ShadowedMemberVariableInheritance _
        | ShadowedMemberFunctionInheritance _ -> true
        | _ -> false

    member inline x.level =
        if x.isWarning then "Warning" else " Error "

    member x.prefix (token: Token) =
        "[" + x.level.PadLeft 7 + "][" + (Utils.unionCaseName x).PadRight 30 + "]["
        + (show token.location.line + ", " + show token.location.column).PadRight 8 + "]: "

    member inline x.show =
        match x with
        | MultiplyDeclaredLocalVariable(token, scopeName) -> x.prefix token + "multiple declared local variable `" + scopeName + "`"
        | MultiplyDeclaredParameter(token, scopeName) -> x.prefix token + "multiple declared parameter `" + scopeName + "`"
        | MultiplyDeclaredFreeFunction(token, scopeName) -> x.prefix token + "multiple declared free function `" + scopeName + "`"
        | MultiplyDeclaredMemberVariable(token, scopeName) -> x.prefix token + "multiple declared member variable `" + scopeName + "`"
        | MultiplyDeclaredMemberFunction(token, scopeName) -> x.prefix token + "multiple declared member function `" + scopeName + "`"
        | MultiplyDeclaredClass(token) -> x.prefix token + "multiple declared class `" + token.lexeme + "`"
        | UndeclaredLocalVariable(token, scopeName) -> x.prefix token + "undeclared local variable `" + scopeName + "`"
        | UndeclaredMemberVariable(token, scopeName) -> x.prefix token + "undeclared member variable `" + scopeName + "`"
        | UndeclaredMemberFunction(token, scopeName) -> x.prefix token + "undeclared member function `" + scopeName + "`"
        | UndeclaredClass(defToken) -> x.prefix defToken + "undeclared class `" + show defToken.lexeme + "`"
        | ArrayDimensionMismatch(defToken, declTable, defType) ->
            x.prefix defToken + "array used with wrong number of dimensions. `" + show (declTable.symbolType @! "") + "` is not compatible with `"
            + String.replaceWith [ "[0]" ] [ "[]" ] (show defType) + "`"
        | _ -> Utils.unionCaseName x

[<RequireQualifiedAccess>]
module Semanter =

    [<RequireQualifiedAccess>]
    module Ast =
        let tryItemType index (tree: Tree<SymbolElement>) =
            match (List.tryItem index tree.children) with
            | Some tree -> tree.root.symbolType
            | None -> None

        let rec firstToken (tree: Tree<SymbolElement>) =
            match tree.children with
            | [] ->
                tree.root.syntaxToken
            | x :: _ -> firstToken x

    module SymbolTableVisitor =
        let visit (table: SymbolTable) (tree: Tree<SymbolElement>) =
            let rec dataMember (tree: Tree<SymbolElement>) =
                // List [ One Id; One IndexList ]
                let idTreeMaybe = List.tryItem 0 tree.children
                let indexListTreeMaybe = List.tryItem 1 tree.children

                let localTableMaybe = table.tryFindLocalTable tree
                let classTableMaybe = table.tryFindClassTable tree
                let classSuperTables = table.findClassTables tree

                match idTreeMaybe, indexListTreeMaybe with
                | Some idTree, Some indexListTree ->
                    let idEntryFromLocalScopeMaybe = localTableMaybe.map (fun it -> it.tryFindTableWithName idTree.root.syntaxToken) @? None
                    let idEntryFromClassScopeMaybe = classTableMaybe.map (fun it -> it.tryFindTableWithName idTree.root.syntaxToken) @? None

                    let idEntryFromSuperScopeMaybe =
                        List.map (fun (it: SymbolTable) -> it.tryFindTableWithName idTree.root.syntaxToken) classSuperTables
                        |> List.choose id
                        |> List.tryLast // Take the closest ancestor

                    match idEntryFromLocalScopeMaybe, idEntryFromClassScopeMaybe, idEntryFromSuperScopeMaybe, indexListTree.children with
                    | Some idEntry, _, _, []
                    | None, Some idEntry, _, []
                    | None, None, Some idEntry, [] ->
                        [], tree <<<< { tree.root with symbolEntry = Some idEntry }
                    | Some idEntry, _, _, indexTrees
                    | None, Some idEntry, _, indexTrees
                    | None, None, Some idEntry, indexTrees ->
                        let errorsAndTreePairs = List.map visitor indexTrees
                        let trees = List.map snd errorsAndTreePairs
                        let errors = List.flatMap fst errorsAndTreePairs
                        let idDeclType = idEntry.symbolType @! "SymbolTableVisitor.visit.dataMember: `idEntry.symbolType` is None"
                        let idDefType = SymbolType.make idDeclType.fakeToken (Some(List.map (fun _ -> 0) indexTrees))

                        if idDefType.dimensionality <> idDeclType.dimensionality
                        then ArrayDimensionMismatch(Ast.firstToken idTree, idEntry, idDefType) :: errors, tree
                        else errors, tree <<<< { tree.root with symbolEntry = Some idEntry.withNoDimensionality }
                    | None, None, None, _ ->
                        let aa = table.findClassTables tree
                        printfn "%s" (show aa)
                        // Assume they are local variables until they get to `varElementList`
                        UndeclaredLocalVariable(idTree.root.syntaxToken, idTree.root.syntaxToken.lexeme) :: [], tree
                | _ -> [], tree

            and varElementList (tree: Tree<SymbolElement>) =
                // Single( ManyOf [ DataMember; FunctionCall ] )
                let undefinedLocalMapper (error: SemanticError) =
                    match error with
                    | UndeclaredLocalVariable(token, scopeName) -> UndeclaredMemberVariable(token, scopeName)
                    | _ -> error

                let errorAndTreePairsMapper (idx: int, (errors: SemanticError list, tree: Tree<SymbolElement>)) =
                    match idx, errors, tree with
                    | 0, [], _ -> []
                    | _, xs, _ -> List.map undefinedLocalMapper xs

                let errorsAndTreePairs = List.map (visitor) tree.children
                let trees = List.map snd errorsAndTreePairs

                let errors =
                    match errorsAndTreePairs with
                    | errorsAndTreePair :: [] -> fst errorsAndTreePair
                    | _ -> List.flatMap errorAndTreePairsMapper (List.indexed errorsAndTreePairs)

                // If last child tree exists, replace the roots symbolEntry content with that child
                errors, (List.tryLast trees <<? (fun t -> tree <<<< { tree.root with symbolEntry = t.root.symbolEntry })) @? tree

            and visitor tree =
                (match tree.root.syntaxElement.syntaxKind with
                 | DataMember ->
                     dataMember
                 | VarElementList ->
                     varElementList
                 | _ ->
                     (fun t ->
                         let errorsAndTreePairs = List.map visitor t.children
                         let trees = List.map snd errorsAndTreePairs
                         let errors = List.flatMap fst errorsAndTreePairs
                         errors, Tree.create t.root trees)) tree

            visitor tree

    [<RequireQualifiedAccess>]
    module TypeCheckVisitor =
        let rec assignStat tree =
            let lhsTypeMaybe = Ast.tryItemType 0 tree
            let rhsTypeMaybe = Ast.tryItemType 1 tree

            match lhsTypeMaybe, rhsTypeMaybe with
            | Some lhsType, Some rhsType ->
                if lhsType.dimensionality <> rhsType.dimensionality then
                    ArrayDimensionMismatch
                        (Ast.firstToken tree,
                         tree.children.[0].root.symbolEntry @! "TypeCheckVisitor.assignStat: `tree.children.[0].root.symbolEntry` is None", rhsType)
                    :: []
                else
                    let isTypeMismatched =
                        match lhsType, rhsType with
                        | Integer _, Integer _
                        | Float _, Float _
                        | Float _, Integer _ -> false
                        | SymbolType.Class(lhsClassName, _), SymbolType.Class(rhsClassName, _) -> lhsClassName <> rhsClassName
                        | _ -> true
                    if isTypeMismatched
                    then TypeMismatch(Ast.firstToken tree, lhsType, rhsType) :: []
                    else []
            | _, _ -> []

        and visit (tree: Tree<SymbolElement>): SemanticError list =
            let visitor =
                match tree.root.syntaxElement.syntaxKind with
                | AssignStat -> assignStat
                | _ -> (fun t -> List.flatMap visit t.children)

            visitor tree

    [<RequireQualifiedAccess>]
    module SymbolCheckVisitor =
        let checkMultiplyDefined: _ list =
            let comparer (a: SymbolTable) (b: SymbolTable) =
                a.name.tokenType = b.name.tokenType

            let xs =
                [ SymbolTable.create "A" (SymbolKind.FreeFunction(SymbolType.create "a1" [], []))
                  SymbolTable.create "B" (SymbolKind.FreeFunction(SymbolType.create "b1" [], []))
                  SymbolTable.create "A" (SymbolKind.FreeFunction(SymbolType.create "a2" [], []))
                  SymbolTable.create "B" (SymbolKind.FreeFunction(SymbolType.create "b2" [], []))
                  SymbolTable.create "E" (SymbolKind.FreeFunction(SymbolType.create "e1" [], []))
                  SymbolTable.create "A" (SymbolKind.FreeFunction(SymbolType.create "a3" [], []))
                  SymbolTable.create "A" (SymbolKind.FreeFunction(SymbolType.create "a4" [], [])) ]

            let mapComparer (a: SymbolTable) (b: SymbolTable) =
                if a.name.tokenType = b.name.tokenType
                then Some(SemanticError.UndeclaredClass(a.name))
                else None

            let a = mapCompare mapComparer xs

            let bb = groupBy comparer xs

            let distinctItems = distinct comparer xs
            distinctItems

        let checkMultiplyDeclared (table: SymbolTable): SemanticError list =
            let distinctEntryMapper (entry: SymbolTable) = entry.duplicateByErrorComparer

            let duplicateErrorMapper (entry: SymbolTable) =
                match entry.kind with
                | Variable _ -> MultiplyDeclaredLocalVariable(entry.name, show entry)
                | Parameter _ -> MultiplyDeclaredParameter(entry.name, show entry)
                | FreeFunction _ -> MultiplyDeclaredLocalVariable(entry.name, show entry)
                | MemberFunction _ -> MultiplyDeclaredMemberFunction(entry.name, show entry)
                | Class _ -> MultiplyDeclaredClass(entry.name)
                | ProgKind
                | Nil -> failwith "Semanter.SymbolCheckVisitor.checkMultiplyDeclared: `ProgKind` or `Nil` type found"

            let duplicateErrorsOuterScope (entries: SymbolTable list) =
                let distinctEntries = List.distinctBy distinctEntryMapper entries
                let duplicateEntries = List.filter (fun it -> not (List.contains it distinctEntries)) entries
                let errors = List.map duplicateErrorMapper duplicateEntries
                errors

            let level1Scopes = [ table.entries ]
            let level2Scopes = List.map (fun it -> it.entries) table.entries
            let level3Scopes = List.map (fun st -> List.map (fun it -> it.entries) st.entries) table.entries

            List.flatMap duplicateErrorsOuterScope (level1Scopes @ level2Scopes)
            @ List.flatMap (fun it -> List.flatMap duplicateErrorsOuterScope it) level3Scopes

        let checkUndefined (table: SymbolTable): SemanticError list =
            []

        let visit (table: SymbolTable): SemanticError list =
            checkMultiplyDeclared table |> (fun errors -> errors @ checkUndefined table)


    let check syntaxTree =
        SymbolTable.makeSymbolTableAndTree syntaxTree
        ||> SymbolTableVisitor.visit
        ||> (fun errors tree -> errors @ TypeCheckVisitor.visit tree, tree)
        ||> (fun errors tree -> errors @ SymbolCheckVisitor.visit (tree.root.symbolEntry @! "Semanter.check: `tree.root.symbolEntry` is None"), tree)
