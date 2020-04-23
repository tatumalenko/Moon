namespace Moon

open FSharpPlus
open System.Collections.Generic

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

    | TypeAssignmentMismatch of Token * string * string // Different types between lhs/rhs
    | TypeDimensionMismatch of Token * Dimensionality * Dimensionality // Different dimensions between decl/def

    | ArrayDimensionMismatch of Token * SymbolTable * SymbolType // Different dimensions between decl/def
    | ArrayDimensionNonPositive of Token * int * int // Declaration uses negative integer
    | ArrayIndexNonInteger of Token * SymbolType // Indexed expr uses non-integer
    | ArrayIndexNonPositive // Indexed expr uses negative integer

    | FunctionWithoutReturn of Token
    | FunctionReturnTypeMismatch of Token * SymbolType * SymbolType
    | FunctionParamTypeMismatch of Token * SymbolType * SymbolType
    | FunctionArityMismatch of Token * int * int
    | FunctionMainWithReturn of Token // ??

    | AddTypeMismatch of Token * SymbolType * SymbolType
    | MultTypeMismatch of Token * SymbolType * SymbolType
    | RelTypeMismatch of Token * SymbolType * SymbolType

    | AddTypeInvalid of Location * SymbolType option * SymbolType option
    | MultTypeInvalid of Location * SymbolType option * SymbolType option //of Token * SymbolType * SymbolType
    | RelTypeInvalid of Location * SymbolType option * SymbolType option //of Token * SymbolType * SymbolType

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

    member x.prefix(token: Token) =
        "[" + x.level.PadLeft 7 + "][" + (Utils.unionCaseName x).PadRight 30 + "]["
        + (show token.location.line + ", " + show token.location.column).PadRight 8 + "]: "

    member x.prefix(location: Location) =
        "[" + x.level.PadLeft 7 + "][" + (Utils.unionCaseName x).PadRight 30 + "][" + (show location.line + ", " + show location.column).PadRight 8
        + "]: "

    member inline x.show =
        match x with
        | MultiplyDeclaredLocalVariable (token, scopeName) -> x.prefix token + "multiple declared local variable `" + scopeName + "`"
        | MultiplyDeclaredParameter (token, scopeName) -> x.prefix token + "multiple declared parameter `" + scopeName + "`"
        | MultiplyDeclaredFreeFunction (token, scopeName) -> x.prefix token + "multiple declared free function `" + scopeName + "`"
        | MultiplyDeclaredMemberVariable (token, scopeName) -> x.prefix token + "multiple declared member variable `" + scopeName + "`"
        | MultiplyDeclaredMemberFunction (token, scopeName) -> x.prefix token + "multiple declared member function `" + scopeName + "`"
        | MultiplyDeclaredClass (token) -> x.prefix token + "multiple declared class `" + token.lexeme + "`"
        | UndeclaredLocalVariable (token, scopeName) -> x.prefix token + "undeclared local variable `" + scopeName + "`"
        | UndeclaredMemberVariable (token, scopeName) -> x.prefix token + "undeclared member variable `" + scopeName + "`"
        | UndeclaredMemberFunction (token, scopeName) -> x.prefix token + "undeclared member function `" + scopeName + "`"
        | UndeclaredClass (defToken) -> x.prefix defToken + "undeclared class `" + show defToken.lexeme + "`"
        | TypeAssignmentMismatch (token, lhsName, rhsName) ->
            x.prefix token + "type assignment mismatch between `" + lhsName + "` and `" + rhsName + "`"
        | ArrayDimensionMismatch (defToken, declTable, defType) ->
            x.prefix defToken + "array used with wrong number of dimensions. `" + show (declTable.symbolType @! "") + "` is not compatible with `"
            + String.replaceWith [ "[0]" ] [ "[]" ] (show defType) + "`"
        | ArrayIndexNonInteger (token, symbolType) -> x.prefix token + "array index invalid, expected integer but called with " + show symbolType
        | FunctionArityMismatch (token, declNumber, callNumber) ->
            x.prefix token + "function arity invalid, expected " + show declNumber + " but called with " + show callNumber + " parameter(s)"
        | FunctionParamTypeMismatch (token, declType, callType) ->
            x.prefix token + "function parameter type mismatch between declared type `" + show declType + "` and called type `" + show callType + "`"
        | AddTypeInvalid (location, lhsType, rhsType) ->
            x.prefix location + "addition operation type invalid involving class types between `" + (lhsType.map show @? "<unresolved type>")
            + "` and `" + (rhsType.map show @? "<unresolved type>") + "`"
        | MultTypeInvalid (location, lhsType, rhsType) ->
            x.prefix location + "multiplication operation type invalid involving class types between `" + (lhsType.map show @? "<unresolved type>")
            + "` and `" + (rhsType.map show @? "<unresolved type>") + "`"
        | RelTypeInvalid (location, lhsType, rhsType) ->
            x.prefix location + "relational operation type invalid involving class types between `" + (lhsType.map show @? "<unresolved type>")
            + "` and `" + (rhsType.map show @? "<unresolved type>") + "`"
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
            let rec num (tree: Tree<SymbolElement>) =
                [], tree

            and addOp (tree: Tree<SymbolElement>) =
                let arithExpr = List.tryItem 0 tree.children
                let term = List.tryItem 1 tree.children

                match arithExpr.map visitor, term.map visitor with
                | Some (lerrors, lhs), Some (rerrors, rhs) ->
                    let errors = lerrors @ rerrors

                    let errors =
                        match lhs.root.symbolType, rhs.root.symbolType with
                        | Some (Integer lhsDims), Some (Integer rhsDims)
                        | Some (Integer lhsDims), Some (Float rhsDims)
                        | Some (Float lhsDims), Some (Integer rhsDims)
                        | Some (Float lhsDims), Some (Float rhsDims) ->
                            match lhs.root.symbolType.Value.dimensionality, rhs.root.symbolType.Value.dimensionality with
                            | Scalar, Scalar ->
                                errors
                            | _ ->
                                SemanticError.AddTypeMismatch(tree.root.syntaxToken, lhs.root.symbolType.Value, rhs.root.symbolType.Value) :: errors
                        | Some (SymbolType.Class (className, _)), _
                        | _, Some (SymbolType.Class (className, _)) ->
                            AddTypeInvalid((Ast.firstToken tree).location, lhs.root.symbolType, rhs.root.symbolType) :: errors
                        | _ -> AddTypeInvalid((Ast.firstToken tree).location, lhs.root.symbolType, rhs.root.symbolType) :: errors

                    // Need to return new tree with child trees with updated symbol entry
                    errors,
                    Tree.create
                        { tree.root with
                              symbolEntry =
                                  Some
                                      { name = tree.root.syntaxToken
                                        kind = lhs.root.symbolEntry.Value.kind
                                        entries = []
                                        tree = Tree.create tree.root.syntaxElement []
                                        globalTree = None } } [ lhs; rhs ]
                | _ -> [], tree

            and multOp (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor with
                | Some (lerrors, lhs), Some (rerrors, rhs) ->
                    let errors = lerrors @ rerrors

                    let errors =
                        match lhs.root.symbolType, rhs.root.symbolType with
                        | Some (Integer lhsDims), Some (Integer rhsDims)
                        | Some (Integer lhsDims), Some (Float rhsDims)
                        | Some (Float lhsDims), Some (Integer rhsDims)
                        | Some (Float lhsDims), Some (Float rhsDims) ->
                            match lhs.root.symbolType.Value.dimensionality, rhs.root.symbolType.Value.dimensionality with
                            | Scalar, Scalar ->
                                errors
                            | _ ->
                                SemanticError.MultTypeMismatch(tree.root.syntaxToken, lhs.root.symbolType.Value, rhs.root.symbolType.Value) :: errors
                        | Some (SymbolType.Class (className, _)), _
                        | _, Some (SymbolType.Class (className, _)) ->
                            MultTypeInvalid((Ast.firstToken tree).location, lhs.root.symbolType, rhs.root.symbolType) :: errors
                        | _ ->
                            MultTypeInvalid((Ast.firstToken tree).location, lhs.root.symbolType, rhs.root.symbolType) :: errors

                    // Need to return new tree with child trees with updated symbol entry
                    errors,
                    Tree.create
                        { tree.root with
                              symbolEntry =
                                  Some
                                      { name = tree.root.syntaxToken
                                        kind = lhs.root.symbolEntry.Value.kind
                                        entries = []
                                        tree = Tree.create tree.root.syntaxElement []
                                        globalTree = None } } [ lhs; rhs ]
                | _ -> [], tree

            and not (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor with
                | Some (errors, factor) ->
                    errors,
                    Tree.create
                        { tree.root with
                              symbolEntry =
                                  Some
                                      { name = tree.root.syntaxToken
                                        kind = factor.root.symbolEntry.Value.kind
                                        entries = []
                                        tree = Tree.create tree.root.syntaxElement []
                                        globalTree = None } } [ factor ]
                | _ -> [], tree

            and sign (tree: Tree<SymbolElement>) =
                not tree // Same as not, i.e. Single (OneOf factor)

            and relExpr (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor,
                      (List.tryItem 2 tree.children).map visitor with
                | Some (lerrors, lhs), Some (merrors, relOp), Some (rerrors, rhs) ->
                    let errors = lerrors @ merrors @ rerrors

                    let errors =
                        match lhs.root.symbolType, rhs.root.symbolType with
                        | Some (Integer lhsDims), Some (Integer rhsDims)
                        | Some (Integer lhsDims), Some (Float rhsDims)
                        | Some (Float lhsDims), Some (Integer rhsDims)
                        | Some (Float lhsDims), Some (Float rhsDims) ->
                            match lhs.root.symbolType.Value.dimensionality, rhs.root.symbolType.Value.dimensionality with
                            | Scalar, Scalar ->
                                errors
                            | x, y when x <> y ->
                                SemanticError.RelTypeMismatch(tree.root.syntaxToken, lhs.root.symbolType.Value, rhs.root.symbolType.Value) :: errors
                            | x, y when x = y ->
                                SemanticError.RelTypeMismatch(tree.root.syntaxToken, lhs.root.symbolType.Value, rhs.root.symbolType.Value) :: errors
                            | _ ->
                                SemanticError.RelTypeMismatch(tree.root.syntaxToken, lhs.root.symbolType.Value, rhs.root.symbolType.Value) :: errors

                        | Some (SymbolType.Class (className, _)), _
                        | _, Some (SymbolType.Class (className, _)) ->
                            SemanticError.RelTypeInvalid((Ast.firstToken tree).location, lhs.root.symbolType, rhs.root.symbolType) :: errors
                        | _ -> SemanticError.RelTypeInvalid((Ast.firstToken tree).location, lhs.root.symbolType, rhs.root.symbolType) :: errors


                    // Need to return new tree with child trees with updated symbol entry
                    errors,
                    Tree.create
                        { tree.root with
                              symbolEntry =
                                  Some
                                      { name = lhs.root.symbolEntry.Value.name
                                        kind = lhs.root.symbolEntry.Value.kind
                                        entries = []
                                        tree = Tree.create tree.root.syntaxElement []
                                        globalTree = None } } [ lhs; relOp; rhs ]
                | _ -> [], tree

            and dataMember (tree: Tree<SymbolElement>) =
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
                    | None, Some idEntry, _, []
                    | None, None, Some idEntry, []
                    | Some idEntry, _, _, [] -> [], tree <<<< { tree.root with symbolEntry = Some idEntry }
                    | None, Some idEntry, _, indexTrees
                    | None, None, Some idEntry, indexTrees
                    | Some idEntry, _, _, indexTrees ->
                        let errorsAndTreePairs = List.map visitor indexTrees
                        let indexTrees = List.map snd errorsAndTreePairs
                        let errors = List.flatMap fst errorsAndTreePairs
                        let idDeclType = idEntry.symbolType @! "SymbolTableVisitor.visit.dataMember: `idEntry.symbolType` is None"
                        let dimensionality = List.length indexTrees
                        let idDefType = SymbolType.make (idDeclType, dimensionality)

                        // Check for same dimensionality
                        if idDefType.dimensionality <> idDeclType.dimensionality then
                            ArrayDimensionMismatch(Ast.firstToken idTree, idEntry, idDefType) :: errors, tree
                        else
                            // Check for integer valued indexing
                            let nonIntegerFolder state (item: Tree<SymbolElement>) =
                                match item.root.symbolType with
                                | Some symbolType ->
                                    match symbolType, symbolType.dimensionality with
                                    | Integer _, Scalar -> state
                                    | _ -> ArrayIndexNonInteger(Ast.firstToken item, symbolType) :: state
                                | None -> state

                            let nonIntegerErrors = List.fold nonIntegerFolder [] indexTrees

                            // Return symbolEntry without dimensionality since we used proper indexing to get non-array type
                            nonIntegerErrors @ errors,
                            Tree.create { tree.root with symbolEntry = Some idEntry.withNoDimensionality } (idTree :: indexTrees)
                    | None, None, None, _ ->
                        // Assume they are local variables until they get to `varElementList`
                        UndeclaredLocalVariable(idTree.root.syntaxToken, idTree.root.syntaxToken.lexeme) :: [], tree
                | _ -> [], tree

            and varElementList (tree: Tree<SymbolElement>): SemanticError list * Tree<SymbolElement> =
                // Single( ManyOf [ DataMember; FunctionCall ] )
                let undefinedLocalMapper (error: SemanticError) =
                    match error with
                    | UndeclaredLocalVariable (token, scopeName) ->
                        UndeclaredMemberVariable(token, scopeName)
                    | _ -> error

                let errorAndTreePairsMapper (idx: int, (errors: SemanticError list, tree: Tree<SymbolElement>)) =
                    match idx, errors, tree with
                    | 0, [], _ -> []
                    | _, xs, _ -> List.map undefinedLocalMapper xs

                let errorsAndTreePairs = List.map visitor tree.children
                let trees = List.map snd errorsAndTreePairs

                let errors =
                    match errorsAndTreePairs with
                    | errorsAndTreePair :: [] -> fst errorsAndTreePair
                    | _ -> List.flatMap errorAndTreePairsMapper (List.indexed errorsAndTreePairs)

                let mutable errors = []

                for (i, errorAndTreePair) in List.indexed errorsAndTreePairs do
                    match i with
                    | 0 ->
                        match List.tryItem 0 trees.[i].children with
                        | None -> errors <- (fst errorAndTreePair) @ errors
                        | Some idNode ->
//                            let localTable, classTable, superTables = table.tryFindTables
//                            let freeTable = table.tryFindTableWithNameAndKind idNode.root.syntaxToken.lexeme SymbolKindType.FreeFunction
                            errors <- (fst errorAndTreePair) @ errors
                            ()
                    | _ ->
                        let classInstance = trees.[i - 1]
                        let classTable = table.tryFindTableWithNameAndKind (classInstance.root.symbolType.map(fun e -> e.className) @? "") SymbolKindType.Class
                        match classTable with
                        | None -> ()
                        | Some classTable ->
                            let idToFind = trees.[i].children.[0].root.syntaxToken.lexeme
                            let symbolKindTypeToFind = match trees.[i].root.syntaxElement.syntaxKind with
                                                        | FunctionCall -> SymbolKindType.MemberFunction
                                                        | _ -> SymbolKindType.Variable

                            let idFinder state (item: SymbolTable) =
                                match item.name.lexeme = idToFind && item.kind.symbolKindType = symbolKindTypeToFind with
                                | true -> true
                                | false ->
                                    if state = true then true else false

                            let idFound = List.fold idFinder false classTable.entries

                            errors <- (fst errorAndTreePair) @ errors

                            errors <- List.filter (fun e -> match e with | UndeclaredLocalVariable _ -> false | _ -> true) errors

                            if idFound
                            then ()
                            else
                                match symbolKindTypeToFind with
                                | SymbolKindType.Variable -> errors <- UndeclaredMemberVariable(Ast.firstToken trees.[i], (Ast.firstToken trees.[i]).lexeme) :: errors
                                | SymbolKindType.MemberFunction -> errors <- UndeclaredMemberFunction(Ast.firstToken trees.[i], (Ast.firstToken trees.[i]).lexeme) :: errors
                                | SymbolKindType.FreeFunction -> errors <- UndefinedFreeFunction(Ast.firstToken trees.[i]) :: errors
                                | _ -> ()


                // If last child tree exists, replace the roots symbolEntry content with that child
                match List.tryLast trees with
                | Some lastTree -> errors, Tree.create { tree.root with symbolEntry = lastTree.root.symbolEntry } trees
                | _ -> errors, tree

            and assignStat (tree: Tree<SymbolElement>): SemanticError list * Tree<SymbolElement> =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor with
                | Some (lerrors, lhs), Some (rerrors, rhs) ->
                    let a = lerrors
                    let b = rerrors
                    let errors = rerrors @ lerrors

                    let errors =
                        match lhs.root.symbolType, rhs.root.symbolType with
                        | Some (Integer ldims), Some (Integer rdims)
                        | Some (Float ldims), Some (Float rdims)
                        | Some (Float ldims), Some (Integer rdims) ->
                            match lhs.root.symbolType.Value.dimensionality, rhs.root.symbolType.Value.dimensionality with
                            | x, y when x = y ->
                                errors
                            | x, y ->
                                TypeDimensionMismatch(lhs.root.symbolType.Value.fakeToken, x, y) :: errors
                        | Some (SymbolType.Class (lname, ldims)), Some (SymbolType.Class (rname, rdims)) ->
                            match lhs.root.symbolType.Value.dimensionality, rhs.root.symbolType.Value.dimensionality with
                            | x, y when x = y && lname = rname -> errors
                            | x, y when x <> y -> TypeDimensionMismatch(lhs.root.symbolType.Value.fakeToken, x, y) :: errors
                            | _ ->
                                // TODO check for common class ancestors
                                TypeAssignmentMismatch(lhs.root.symbolType.Value.fakeToken, lname, rname) :: errors
                        | _ -> errors
                    errors, Tree.create tree.root [ lhs; rhs ]
                | _ -> [], tree

            and functionCall (tree: Tree<SymbolElement>): SemanticError list * Tree<SymbolElement> =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor with
                | Some (idErrors, idNode), Some (aParamListErrors, aParamList) ->
                    let errors = idErrors @ aParamListErrors
                    let freeFunctionTable = table.tryFindTableWithNameAndKind idNode.root.syntaxToken.lexeme SymbolKindType.FreeFunction
                    let memberFunctionTable = table.tryFindTableWithNameAndKind idNode.root.syntaxToken.lexeme SymbolKindType.MemberFunction
                    let functionTable = table.tryFindFunctionTable tree
                    match freeFunctionTable, memberFunctionTable with
                    | Some functionTable, _
                    | _, Some functionTable ->
                        // Lift return type and assign to type of this tree
                        let newTree = Tree.create { tree.root with symbolEntry = Some functionTable } [ idNode; aParamList ]

                        // Check for number of params called with
                        match functionTable.numberOfParameters = List.length aParamList.children with
                        | false ->
                            SemanticError.FunctionArityMismatch
                                (Ast.firstToken tree, functionTable.numberOfParameters, List.length aParamList.children) :: errors, newTree
                        | true ->
                            // Check for correct param types used
                            let callParamTypes =
                                (List.map (fun (e: Tree<SymbolElement>) -> e.root.symbolType) aParamList.children) |> List.choose id
                            let declParamTypes = functionTable.paramTypes

                            let wrongParamTypeFolder state (item: SymbolType * SymbolType) =
                                let declParamType = fst item
                                let callParamType = snd item
                                match declParamType.dimensionality = callParamType.dimensionality with
                                | true -> state
                                | false -> FunctionParamTypeMismatch(Ast.firstToken tree, declParamType, callParamType) :: state
                            match List.length declParamTypes = List.length callParamTypes with
                            | false -> errors, newTree
                            | true ->
                                let declAndCallParamTypePairs =
                                    List.map2 (fun declParamType callParamType -> declParamType, callParamType) declParamTypes callParamTypes
                                let paramTypeErrors = List.fold wrongParamTypeFolder [] declAndCallParamTypePairs
                                paramTypeErrors @ errors, newTree
                    | _ -> errors, tree
                | _ -> [], tree

            and other (tree: Tree<SymbolElement>): SemanticError list * Tree<SymbolElement> =
                let errorsAndTreePairs = List.map visitor tree.children
                let trees = List.map snd errorsAndTreePairs
                let errors = List.flatMap fst errorsAndTreePairs
                errors, Tree.create tree.root trees

            and visitor (tree: Tree<SymbolElement>): SemanticError list * Tree<SymbolElement> =
                tree
                |> match tree.root.syntaxElement.syntaxKind with
                   | Num -> num
                   | AddOp -> addOp
                   | MultOp -> multOp
                   | Not -> not
                   | Sign -> sign
                   | RelExpr -> relExpr
                   | DataMember -> dataMember
                   | VarElementList -> varElementList
                   | AssignStat -> assignStat
                   | FunctionCall -> functionCall
                   | _ -> other

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
                        | SymbolType.Class (lhsClassName, _), SymbolType.Class (rhsClassName, _) -> lhsClassName <> rhsClassName
                        | _ -> true
                    if isTypeMismatched
                    then TypeAssignmentMismatch(Ast.firstToken tree, show lhsType, show rhsType) :: []
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

            let bb = Extensions.groupBy comparer xs

            let distinctItems = Extensions.distinct comparer xs
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

    type MemoryElement =
        { tag: string
          size: int }

        static member empty =
            { tag = ""
              size = 0 }

        static member make tag size =
            { tag = tag
              size = size }

        static member sizeOf bytesPerType dimList = bytesPerType * (List.fold (fun state item -> state * item) 1 dimList)

    type CodeElement =
        { symbolElement: SymbolElement
          memoryElement: MemoryElement }
        static member make se me =
            { symbolElement = se
              memoryElement = me }

    type CodeTable =
        { symbolTable: SymbolTable
          memoryElement: MemoryElement
          entries: CodeTable list }
        static member make st me cts =
            { symbolTable = st
              memoryElement = me
              entries = cts }

    [<StructuredFormatDisplay("{show}")>]
    type CodeFactory =
        { mutable exec: string
          mutable data: string
          mutable stack: MemoryElement list
          mutable registry: Dictionary<Tree<SymbolElement>, MemoryElement>
          mutable symbolRegistry: Dictionary<SymbolTable, MemoryElement>
          mutable count: int
          mutable labelCount: int
          mutable registers: string list }

        static member empty =
            { exec = ""
              data = ""
              stack = []
              registry = Dictionary()
              symbolRegistry = Dictionary()
              count = 0
              labelCount = 0
              registers = [ "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"; "r11"; "r12"; "r13"; "r14"; "r15" ] }

        static member combine(xs: CodeFactory list) =
            let ce = CodeFactory.empty

            let combiner state item =
                &state.exec += sprintf "%s\n" item.exec
                &state.data += sprintf "%s\n" item.data
                state.stack <- state.stack @ item.stack
                state.count <- max state.count item.count
                state
            List.fold combiner ce xs

        member private x.makeTag =
            &x.count += 1
            "t" + show x.count

        member x.makeLabel =
            &x.labelCount += 1
            "l" + show x.labelCount

        member x.pop =
            match x.registers with
            | y :: ys ->
                x.registers <- ys
                y
            | [] -> ""

        member x.push y = x.registers <- y :: x.registers

        member x.code(opCode: string) = x.code ("", opCode, "", "")

        member x.code(opCode: string, opArgs: string) = x.code ("", opCode, opArgs, "")

        member x.code(opCode: string, opArgs: string, comment: string) = x.code ("", opCode, opArgs, comment)

        member x.code(label: string, opCode: string, opArgs: string, comment: string) =
            &x.exec += sprintf "%-10s%-10s%-20s%s\n" label opCode opArgs (if comment = "" then "" else "% " + (String.truncate 200 comment))

        member private x.register (me: MemoryElement) (comment: string) =
            x.stack <- me :: x.stack
            &x.data += sprintf "%-10s%-10s%-5i%s\n" me.tag "res" me.size (if comment = "" then "" else "% " + (String.truncate 200 comment))

        member x.alloc(tag: string, size: int) = x.alloc (tag, size, "")

        member x.alloc(tag: string, size: int, comment: string) =
            let me = MemoryElement.make tag size
            x.register me comment
            me

        member x.get(tag: string) = List.tryFind (fun e -> e.tag = tag) x.stack @! "CodeGenerator.get: tried to get but was not found"

        member x.getOrAlloc(tree: Tree<SymbolElement>) = x.getOrAlloc (tree, "")

        member x.getOrAlloc(tree: Tree<SymbolElement>, comment: string) =
            let me =
                match tree.root.symbolEntry with
                | Some entry ->
                    // Check if symbol table associated scope wise
                    match x.symbolRegistry.ContainsKey(entry) with
                    | true ->
                        // Check to see if exists already and use that MemoryElement
                        x.symbolRegistry.GetValueOrDefault(entry, MemoryElement.empty)
                    | false ->
                        // If not, create a new kv entry
                        let me = MemoryElement.make x.makeTag (x.sizeOf entry.kind)
                        x.symbolRegistry.Add(entry, me)
                        x.register me (show tree)
                        me
                | None ->
                    // If no symbol table associated, use tree registry
                    match x.registry.ContainsKey(tree) with
                    | true ->
                        // Check to see if exists already and use that MemoryElement
                        x.registry.GetValueOrDefault(tree, MemoryElement.empty)
                    | false ->
                        // If not, create a new kv entry
                        let me = MemoryElement.make x.makeTag (x.sizeOf tree.root.syntaxElement)
                        x.registry.Add(tree, me)
                        x.register me (show tree)
                        me

            me

        member x.comment(s: string) =
            &x.exec += sprintf "%-10s%s%s\n" "" "% " s

        member x.sizeOf(kind: SymbolKind) =
            match kind with
            | Variable symbolType
            | Parameter symbolType ->
                match symbolType with
                | Integer dimList ->
                    MemoryElement.sizeOf 4 (dimList @? [])
                | Float dimList ->
                    MemoryElement.sizeOf 8 (dimList @? [])
                | SymbolType.Class (className, dimList) ->
                    MemoryElement.sizeOf 20 (dimList @? [])
                | Void
                | SymbolType.Nil ->
                    0
            | FreeFunction (returnType, paramTypes) ->
                MemoryElement.sizeOf 0 []
            | MemberFunction (returnType, paramTypes, classType) ->
                MemoryElement.sizeOf 0 []
            | Class superTypes ->
                MemoryElement.sizeOf 0 []
            | ProgKind
            | Nil ->
                0

        member x.sizeOf(syntaxElement: SyntaxElement) =
            match syntaxElement.syntaxKind with
            | Num ->
                match syntaxElement.token.map (fun e -> e.lexeme) with
                | Some (Int x) ->
                    4
                | Some (Double x) ->
                    8
                | _ -> failwith "unexpected Num case"
            | _ -> 0

        member x.show = x.exec + "\n" + x.data + sprintf "%-10s%-10s%-5s%s" "space" "db" "\" \"" "% Separator for console output"

    [<RequireQualifiedAccess>]
    module CodeGenerationVisitor =
        let visit (tree: Tree<SymbolElement>) =
            let mutable cf = CodeFactory.empty


            let rec num (tree: Tree<SymbolElement>) =
                let me = cf.getOrAlloc tree
                let r = cf.pop
                cf.code ("addi", sprintf "%s,%s,%s" r "r0" tree.root.syntaxToken.lexeme, show tree)
                cf.code ("sw", sprintf "%s(r0),%s" me.tag r)
                cf.push r
                me

            and writeStat (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor with
                | Some expr ->
                    let r = cf.pop
                    cf.code ("lw", sprintf "%s,%s(r0)" r expr.tag, show tree)
                    cf.code ("sw", sprintf "-8(r14),%s" r)
                    cf.code ("addi", sprintf "%s,r0,buf" r)
                    cf.code ("sw", sprintf "-12(r14),%s" r)
                    cf.code ("jl", "r15,intstr")
                    cf.code ("sw", "-8(r14),r13")
                    cf.code ("jl", "r15,putstr")
                    // Add space separator
                    cf.code ("lb", sprintf "%s,%s(r0)" r "space")
                    cf.code ("putc", sprintf "%s" r)
                    cf.push r
                    expr
                | _ -> failwith "CodeGeneratorVisitor.visit.writeStat: expected `Some expr` but was `None`"

            and dataMember (tree: Tree<SymbolElement>) =
                cf.getOrAlloc tree

            and varElementList (tree: Tree<SymbolElement>) =
                cf.getOrAlloc tree

            and relExpr (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor, List.tryItem 1 tree.children, (List.tryItem 2 tree.children).map visitor with
                | Some lhs, Some relOp, Some rhs ->
                    let me = cf.getOrAlloc tree

                    let op =
                        match relOp.root.syntaxToken.tokenType with
                        | EqualEqual -> Some "ceq"
                        | LtGt -> Some "cne"
                        | Lt -> Some "clt"
                        | LtEqual -> Some "cle"
                        | Gt -> Some "cgt"
                        | GtEqual -> Some "cge"
                        | _ -> None
                    match op with
                    | Some opCode ->
                        let r1 = cf.pop
                        let r2 = cf.pop
                        let r3 = cf.pop
                        cf.code ("lw", sprintf "%s,%s(r0)" r1 lhs.tag, show tree)
                        cf.code ("lw", sprintf "%s,%s(r0)" r2 rhs.tag)
                        cf.code (opCode, sprintf "%s,%s,%s" r3 r1 r2)
                        cf.code ("sw", sprintf "%s(r0),%s" me.tag r3)
                        cf.push r3
                        cf.push r2
                        cf.push r1
                        me
                    | None -> me
                | _ -> failwith "CodeGeneratorVisitor.visit.relExpr: expected `Some arithExpr, relOp, arithExpr` but was `None`"

            and addOp (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor with
                | Some lhs, Some rhs ->
                    let me = cf.getOrAlloc tree

                    let op =
                        match tree.root.syntaxToken.tokenType with
                        | Plus -> Some(Plus, "add")
                        | Minus -> Some(Minus, "sub")
                        | Or -> Some(Or, "")
                        | _ -> None
                    match op with
                    | Some (Plus, opCode)
                    | Some (Minus, opCode) ->
                        let r1 = cf.pop
                        let r2 = cf.pop
                        let r3 = cf.pop
                        cf.code ("lw", sprintf "%s,%s(r0)" r1 rhs.tag, show tree)
                        cf.code ("lw", sprintf "%s,%s(r0)" r2 lhs.tag)
                        cf.code (opCode, sprintf "%s,%s,%s" r3 r2 r1)
                        cf.code ("sw", sprintf "%s(r0),%s" me.tag r3)
                        cf.push r3
                        cf.push r2
                        cf.push r1
                        me
                    | Some (Or, _) ->
                        let notzero = cf.makeLabel
                        let endor = cf.makeLabel
                        let r1 = cf.pop
                        let r2 = cf.pop
                        let r3 = cf.pop
                        cf.code ("lw", sprintf "%s,%s(r0)" r1 lhs.tag, show tree)
                        cf.code ("lw", sprintf "%s,%s(r0)" r2 rhs.tag)
                        cf.code ("bnz", sprintf "%s,%s" r1 notzero)
                        cf.code ("bnz", sprintf "%s,%s" r2 notzero)
                        cf.code ("addi", sprintf "%s,r0,0" r3)
                        cf.code ("j", sprintf "%s" endor)
                        cf.code (notzero, "addi", sprintf "%s,r0,1" r3, "")
                        cf.code (endor, "sw", sprintf "%s(r0),%s" me.tag r3, "")
                        cf.push r3
                        cf.push r2
                        cf.push r1
                        me
                    | _ -> me
                | _ -> failwith "CodeGeneratorVisitor.visit.addOp: expected `Some arithExpr, term` but was `None`"

            and multOp (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor with
                | Some lhs, Some rhs ->
                    let me = cf.getOrAlloc tree

                    let op =
                        match tree.root.syntaxToken.tokenType with
                        | Asterisk -> Some(Asterisk, "mul")
                        | Slash -> Some(Slash, "div")
                        | And -> Some(And, "")
                        | _ -> None
                    match op with
                    | Some (Asterisk, opCode)
                    | Some (Slash, opCode) ->
                        let r1 = cf.pop
                        let r2 = cf.pop
                        let r3 = cf.pop
                        cf.code ("lw", sprintf "%s,%s(r0)" r1 rhs.tag, show tree)
                        cf.code ("lw", sprintf "%s,%s(r0)" r2 lhs.tag)
                        cf.code (opCode, sprintf "%s,%s,%s" r3 r2 r1)
                        cf.code ("sw", sprintf "%s(r0),%s" me.tag r3)
                        cf.push r3
                        cf.push r2
                        cf.push r1
                        me
                    | Some (And, _) ->
                        let zero = cf.makeLabel
                        let endand = cf.makeLabel
                        let r1 = cf.pop
                        let r2 = cf.pop
                        let r3 = cf.pop
                        cf.code ("lw", sprintf "%s,%s(r0)" r1 lhs.tag, show tree)
                        cf.code ("lw", sprintf "%s,%s(r0)" r2 rhs.tag)
                        cf.code ("bz", sprintf "%s,%s" r1 zero)
                        cf.code ("bz", sprintf "%s,%s" r2 zero)
                        cf.code ("addi", sprintf "%s,r0,1" r3)
                        cf.code ("j", sprintf "%s" endand)
                        cf.code (zero, "addi", sprintf "%s,r0,0" r3, "")
                        cf.code (endand, "sw", sprintf "%s(r0),%s" me.tag r3, "")
                        cf.push r3
                        cf.push r2
                        cf.push r1
                        me
                    | _ -> me
                | _ -> failwith "CodeGeneratorVisitor.visit.multOp: expected `Some term, factor` but was `None`"

            and sign (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor with
                | Some factor ->
                    let me = cf.getOrAlloc tree

                    let op =
                        match tree.root.syntaxToken.tokenType with
                        | Plus -> Some "add"
                        | Minus -> Some "sub"
                        | _ -> None
                    match op with
                    | Some opCode ->
                        let r1 = cf.pop
                        let r2 = cf.pop
                        cf.code ("lw", sprintf "%s,%s(r0)" r1 factor.tag, show tree)
                        cf.code (opCode, sprintf "%s,r0,%s" r2 r1)
                        cf.code ("sw", sprintf "%s(r0),%s" me.tag r2)
                        cf.push r2
                        cf.push r1
                        me
                    | None -> me
                | _ -> failwith "CodeGeneratorVisitor.visit.sign: expected `Some factor` but was `None`"

            and not (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor with
                | Some factor ->
                    let me = cf.getOrAlloc tree
                    let notzero = cf.makeLabel
                    let endnot = cf.makeLabel
                    let r1 = cf.pop
                    cf.code ("lw", sprintf "%s,%s(r0)" r1 factor.tag, show tree)
                    cf.code ("bnz", sprintf "%s,%s" r1 notzero)
                    cf.code ("addi", sprintf "%s,r0,1" r1)
                    cf.code ("sw", sprintf "%s(r0),%s" me.tag r1)
                    cf.code ("j", sprintf "%s" endnot)
                    cf.code (notzero, "sw", sprintf "%s(r0),r0" me.tag, "")
                    cf.code (endnot, "", "", "")
                    me
                | _ -> failwith "CodeGeneratorVisitor.visit.not: expected `Some factor` but was `None`"

            and assignStat (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor with
                | Some lhs, Some rhs ->
                    let r = cf.pop
                    cf.code ("lw", sprintf "%s,%s(r0)" r rhs.tag, show tree)
                    cf.code ("sw", sprintf "%s(r0),%s" lhs.tag r)
                    cf.push r
                    lhs
                | _ -> failwith "CodeGeneratorVisitor.visit.assignStat: expected `Some varElementList, expr` but was `None`"

            and classDeclList (tree: Tree<SymbolElement>) =
                let mes = List.map visitor tree.children
                MemoryElement.empty

            and funcDefList (tree: Tree<SymbolElement>) =
                let ces = List.map visitor tree.children
                MemoryElement.empty

            and mainFuncBody (tree: Tree<SymbolElement>) =
                cf.code "entry"
                cf.code ("addi", "r14,r0,topaddr")
                let mes = List.map visitor tree.children
                cf.alloc ("buf", 20, "Buffer space used for console output") |> ignore
                cf.code "hlt"
                MemoryElement.empty

            and prog (tree: Tree<SymbolElement>) =
                match (List.tryItem 0 tree.children).map visitor, (List.tryItem 1 tree.children).map visitor,
                      (List.tryItem 2 tree.children).map visitor with
                | Some classDeclList, Some funcDeclList, Some mainFuncBody ->
                    MemoryElement.empty
                | _ -> MemoryElement.empty

            and visitor (tree: Tree<SymbolElement>) =
                match tree.root.syntaxElement.syntaxKind with
                | Prog -> prog tree
                | ClassDeclList -> classDeclList tree
                | FuncDefList -> funcDefList tree
                | MainFuncBody -> mainFuncBody tree
                | AssignStat -> assignStat tree
                | VarElementList -> varElementList tree
                | RelExpr -> relExpr tree
                | AddOp -> addOp tree
                | MultOp -> multOp tree
                | Sign -> sign tree
                | Not -> not tree
                | DataMember -> dataMember tree
                | WriteStat -> writeStat tree
                | Num -> num tree
                | _ -> MemoryElement.empty

            visitor tree |> ignore

            cf

    let check syntaxTree =
        SymbolTable.makeSymbolTableAndTree syntaxTree
        ||> SymbolTableVisitor.visit
        ||> (fun errors tree -> errors @ TypeCheckVisitor.visit tree, tree)
        ||> (fun errors tree -> SymbolCheckVisitor.visit (tree.root.symbolEntry @? SymbolTable.empty) @ errors, tree)
