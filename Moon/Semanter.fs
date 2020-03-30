namespace Moon

[<StructuredFormatDisplay("{show}")>]
type SemanticError =
    // Symbol table phase -- creation
    | MultiplyDeclaredLocalVariable of Token * string * string
    | MultiplyDeclaredFreeFunction of Token * string * string // Same name and type signatures (not overloaded)
    | MultiplyDeclaredMemberVariable of Token * string * string
    | MultiplyDeclaredMemberFunction of Token * string * string // Same name and type signatures (not overloaded)
    | MultiplyDeclaredClass of Token * SymbolType * SymbolType

    | UndefinedFreeFunction of Token
    | UndefinedMemberVariable of Token * SymbolType
    | UndefinedMemberFunction of Token * SymbolType

    // Semantic check phase -- binding and type checking
    | UndeclaredLocalVariable of Token * string
    | UndeclaredMemberVariable of Token * SymbolType
    | UndeclaredMemberFunction of Token * SymbolType
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

    member x.isWarning =
        match x with
        | OverloadedFreeFunction _
        | OverloadedMemberFunction _
        | ShadowedMemberVariableInheritance _
        | ShadowedMemberFunctionInheritance _ -> true
        | _ -> false

    member inline x.show =
        match x with
        | ArrayDimensionMismatch(defToken, declTable, defType) ->
            "Array dimension mismatch: " + show (declTable.symbolType @! "") + " is not compatible with " + show defType + "\n"
            + show defToken.location
        | _ -> ""

type SemanterSymbolTableContext =
    { globalSymbolTable: SymbolTable
      localSymbolTable: SymbolTable }

[<RequireQualifiedAccess>]
module Semanter =
    open FSharpPlus

    let squareBracketCount typeStr =
        String.split [ "[" ] typeStr
        |> Seq.length
        |> (-) 1
        |> abs

    let semanticErrors results =
        let f xs =
            match xs with
            | Ok _ -> false
            | Error _ -> true
        List.filter f results

    [<RequireQualifiedAccess>]
    module Ast =
        let typeAt index (symbolTree: Tree<SymbolElement>) =
            match (List.tryItem index symbolTree.children) with
            | Some tree -> tree.root.symbolType
            | None -> None

        let rec firstToken symbolTree =
            match symbolTree.children with
            | [] ->
                symbolTree.root.syntaxElement.token @! "Semanter.firstToken: Tried to get `Tree<SymbolElement>.root.syntaxElement.token` but was None"
            | x :: _ -> firstToken x

    module SymbolTableVisitor =
        let flatMapFstOfTriples xs = List.flatMap (fun (a, _, _) -> a) xs

        let mapTrdOfTriples xs = List.map (fun (_, _, c) -> c) xs

        //        let rec dataMember (errors: SemanticError list) (table: SymbolTable) (tree: Tree<SymbolElement>) =
        //                // List [ One Id; One IndexList ]
        //                let idTreeMaybe = List.tryItem 0 tree.children
        //                let indexListTreeMaybe = List.tryItem 1 tree.children
        //
        //                let localScopeTableMaybe = table.tryFindLocalTable tree
        //                let classScopeTableMaybe = table.tryFindClassTable tree
        //
        //                match idTreeMaybe, indexListTreeMaybe with
        //                | Some idTree, Some indexListTree ->
        //                    let idEntryFromLocalScopeMaybe = localScopeTableMaybe.map (fun it -> it.tryFindTableWithName idTree.root.syntaxToken) @? None
        //                    let idEntryFromClassScopeMaybe = classScopeTableMaybe.map (fun it -> it.tryFindTableWithName idTree.root.syntaxToken) @? None
        //
        //                    match idEntryFromLocalScopeMaybe, idEntryFromClassScopeMaybe, indexListTree.children with
        //                    | Some idEntry, _, [] ->
        //                        let newSymbolTree = tree <<<< { tree.root with symbolEntry = Some idEntry }
        //                        errors, table, newSymbolTree
        //                    | Some idEntry, _, indexTrees
        //                    | None, Some idEntry, indexTrees ->
        //                        let errorsAndTableAndTreeTriples = List.map (makeSymbolTableAndTree [] table) indexTrees
        //                        let idDeclType = idEntry.symbolType @! "idEntry.symbolType was None"
        //                        let idDefType = SymbolType.make idDeclType.fakeToken (Some (List.map (fun _ -> 0) indexTrees))
        //
        //                        if (idDefType.dimensionality <> idDeclType.dimensionality)
        //                        then ArrayDimensionMismatch(Ast.firstToken idTree, idEntry, idDefType) :: flatMapFstOfTriples errorsAndTableAndTreeTriples @ errors, table, tree
        //                        else
        //                            let newSymbolTree = tree <<<< { tree.root with symbolEntry = Some idEntry.withNoDimensionality }
        //                            flatMapFstOfTriples errorsAndTableAndTreeTriples @ errors, table, newSymbolTree
        //                    | None, None, _ ->
        //                        UndeclaredLocalVariable(idTree.root.syntaxToken, idTree.root.syntaxToken.lexeme) :: errors, table, tree
        //                | _ -> errors, table, tree
        //
        //        and varElementList (errors: SemanticError list) (table: SymbolTable) (tree: Tree<SymbolElement>) =
        //            // Single( ManyOf [ DataMember; FunctionCall ] )
        //                let errorsAndTableAndTreeTriples = List.map (makeSymbolTableAndTree [] table) tree.children
        //                let trees = mapTrdOfTriples errorsAndTableAndTreeTriples
        //                let rightMostChildTree = List.tryLast trees
        //                let newSymbolTree = rightMostChildTree <?> (fun t -> tree <<<< { tree.root with symbolEntry = t.root.symbolEntry })
        //                flatMapFstOfTriples errorsAndTableAndTreeTriples @ errors, table, newSymbolTree @? tree
        let rec dataMember (table: SymbolTable) (tree: Tree<SymbolElement>) =
            // List [ One Id; One IndexList ]
            let idTreeMaybe = List.tryItem 0 tree.children
            let indexListTreeMaybe = List.tryItem 1 tree.children

            let localScopeTableMaybe = table.tryFindLocalTable tree
            let classScopeTableMaybe = table.tryFindClassTable tree

            match idTreeMaybe, indexListTreeMaybe with
            | Some idTree, Some indexListTree ->
                let idEntryFromLocalScopeMaybe = localScopeTableMaybe.map (fun it -> it.tryFindTableWithName idTree.root.syntaxToken) @? None
                let idEntryFromClassScopeMaybe = classScopeTableMaybe.map (fun it -> it.tryFindTableWithName idTree.root.syntaxToken) @? None

                match idEntryFromLocalScopeMaybe, idEntryFromClassScopeMaybe, indexListTree.children with
                | Some idEntry, _, [] ->
                    let newSymbolTree = tree <<<< { tree.root with symbolEntry = Some idEntry }
                    [], table, newSymbolTree
                | Some idEntry, _, indexTrees
                | None, Some idEntry, indexTrees ->
                    let errorsAndTableAndTreeTriples = List.map (makeSymbolTableAndTree table) indexTrees
                    let idDeclType = idEntry.symbolType @! "idEntry.symbolType was None"
                    let idDefType = SymbolType.make idDeclType.fakeToken (Some(List.map (fun _ -> 0) indexTrees))

                    if (idDefType.dimensionality <> idDeclType.dimensionality) then
                        ArrayDimensionMismatch(Ast.firstToken idTree, idEntry, idDefType) :: flatMapFstOfTriples errorsAndTableAndTreeTriples, table,
                        tree
                    else
                        let newSymbolTree = tree <<<< { tree.root with symbolEntry = Some idEntry.withNoDimensionality }
                        flatMapFstOfTriples errorsAndTableAndTreeTriples, table, newSymbolTree
                | None, None, _ ->
                    UndeclaredLocalVariable(idTree.root.syntaxToken, idTree.root.syntaxToken.lexeme) :: [], table, tree
            | _ -> [], table, tree

        and varElementList (table: SymbolTable) (tree: Tree<SymbolElement>) =
            // Single( ManyOf [ DataMember; FunctionCall ] )
            let errorsAndTableAndTreeTriples = List.map (makeSymbolTableAndTree table) tree.children
            let trees = mapTrdOfTriples errorsAndTableAndTreeTriples
            let rightMostChildTree = List.tryLast trees
            let newSymbolTree = rightMostChildTree <?> (fun t -> tree <<<< { tree.root with symbolEntry = t.root.symbolEntry })
            flatMapFstOfTriples errorsAndTableAndTreeTriples, table, newSymbolTree @? tree

        and makeSymbolTableAndTree (table: SymbolTable) (tree: Tree<SymbolElement>) =
            match tree.root.syntaxElement.syntaxKind with
            | DataMember ->
                dataMember table tree
            | VarElementList ->
                varElementList table tree
            | _ ->
                let errorsAndTableAndTreeTriples = List.map (makeSymbolTableAndTree table) tree.children
                let trees = mapTrdOfTriples errorsAndTableAndTreeTriples
                flatMapFstOfTriples errorsAndTableAndTreeTriples, table, Tree.create tree.root trees

        let visit syntaxTree =
            let symbolTable, symbolTree = SymbolTable.makeSymbolTableAndTree syntaxTree
            makeSymbolTableAndTree symbolTable symbolTree

    [<RequireQualifiedAccess>]
    module TypeCheckVisitor =
        let rec assignStat tree =
            let a = Ast.typeAt 0 tree
            let b = Ast.typeAt 1 tree

            match a, b with
            | Some lhs, Some rhs ->
                if lhs.dimensionality <> rhs.dimensionality then
                    ArrayDimensionMismatch(Ast.firstToken tree, tree.children.[0].root.symbolEntry @! "", rhs) :: []
                else
                    let isTypeMismatched =
                        match lhs, rhs with
                        | Integer _, Integer _
                        | Float _, Float _
                        | Float _, Integer _ -> false
                        | SymbolType.Class(lhsClassName, _), SymbolType.Class(rhsClassName, _) -> lhsClassName <> rhsClassName
                        | _ -> true
                    if isTypeMismatched
                    then TypeMismatch(Ast.firstToken tree, lhs, rhs) :: []
                    else []
            | _, _ -> []

        and visit (tree: Tree<SymbolElement>): SemanticError list =
            let visitor =
                match tree.root.syntaxElement.syntaxKind with
                | AssignStat -> assignStat
                | _ -> (fun t -> List.flatMap (visit) t.children)

            visitor tree

    let check tree =
        let symTa1, symTr1 = SymbolTable.makeSymbolTableAndTree tree
        let symErs, symTa, symTr = SymbolTableVisitor.visit tree
        let typErs = TypeCheckVisitor.visit symTr

        tree
        |> SymbolTableVisitor.visit
        |||> (fun symbolErrors symbolTable symbolTree -> symbolTable, symbolTree, symbolErrors @ TypeCheckVisitor.visit symbolTree)
        |||> (fun symbolTable symbolTree typeErrors -> typeErrors, symbolTable, symbolTree)
