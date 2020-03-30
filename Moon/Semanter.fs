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
        | ArrayDimensionMismatch(defToken, declTable, defType) -> "Array dimension mismatch: " + show (declTable.symbolType @! "") + " is not compatible with " + show defType + "\n" + show defToken.location
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
        let rec findSymbolEntryFromToken (idSyntaxToken: Token) (symbolTable: SymbolTable) =
            let rec f x =
                let idSyntaxLexeme = idSyntaxToken.lexeme

                match x with
                | Some st when st.name.lexeme = idSyntaxLexeme -> x
                | Some st when st.name.lexeme <> idSyntaxLexeme ->
                    let xs = List.map (f << Some) st.entries
                    let x = List.tryFind Option.isSome xs @? None
                    x
                | _ -> None
            f (Some symbolTable)

        let errorsFromTriples xs = List.flatMap (fun (a, b,c ) -> a) xs

        let treesFromTriples xs =  List.map (fun (a, b,c ) -> c) xs

        let rec makeSymbolTableAndTree (errors: SemanticError list) (symbolTable: SymbolTable) (symbolTree: Tree<SymbolElement>) =
            let root = symbolTree.root

            let children = symbolTree.children
//            let localScopeTable =
//                SymbolTable.findSymbolTable symbolTree globalTree
//                @! "Semanter.SymbolTableVisitor.makeSymbolTableAndTree: Tried to get `localScopeTable` but was None"

            match symbolTree.root.syntaxElement.syntaxKind with
//            |
//            | FuncDef ->
//                let trees = List.map (trd << (makeSymbolTableAndTree [] symbolTable)) children
//                errors, symbolTable, Tree.create symbolTree.root trees
//            | MainFuncBody ->
//                //SymbolTable.findSymbolTable [] symbolTree
//                let trees = List.map (trd << (makeSymbolTableAndTree [] symbolTable)) children
//                errors, symbolTable, Tree.create symbolTree.root trees
            | DataMember ->
                // List [ One Id; One IndexList ]
                let idTreeMaybe = List.tryItem 0 children
                let indexListTreeMaybe = List.tryItem 1 children

                // Local relative to where symbolTree is (DataMember node)
                let localScopeTable = symbolTable.findLocalSymbolTable symbolTree @! "Semanter.SymbolTableVisitor.makeSymbolTableAndTree: Tried to get `localScopeTable` but was None"
                let memberScopeTable = symbolTable.findClassSymbolTable symbolTree

                match idTreeMaybe, indexListTreeMaybe with
                | Some idTree, Some indexListTree ->
                    let idEntryMaybe = findSymbolEntryFromToken idTree.root.syntaxToken localScopeTable
                    let idEntryFromClassMaybe = memberScopeTable.map (fun it -> findSymbolEntryFromToken idTree.root.syntaxToken it) @? None


                    match idEntryMaybe, idEntryFromClassMaybe, indexListTree.children with
                    | Some idEntry, _, [] ->
                        let newSymbolTree = symbolTree <<<< { root with symbolEntry = idEntryMaybe }
                        errors, symbolTable, newSymbolTree
                    | Some idEntry, _, indexTrees
                    | None, Some idEntry, indexTrees ->
                        // Check indexListChildren
                        let aaa = List.map (makeSymbolTableAndTree [] symbolTable) indexTrees
                        let idDeclType = idEntry.symbolType @! "idEntry.symbolType was None"
                        let idDimensions = idDeclType.dimensions
                        let idDefType = SymbolType.make idDeclType.fakeToken (Some (List.map (fun _ -> 0) indexTrees))

                        if (idDefType.dimensionality <> idDeclType.dimensionality)
                        then ArrayDimensionMismatch(Ast.firstToken idTree, idEntry, idDefType) :: (errorsFromTriples aaa) @ errors, symbolTable, symbolTree
                        else
                            let newSymbolTree = symbolTree <<<< { root with symbolEntry = Some idEntry.withNoDimensionality }
                            (errorsFromTriples aaa) @ errors, symbolTable, newSymbolTree
                    | None, None, _ ->
                        UndeclaredLocalVariable(idTree.root.syntaxToken, localScopeTable.name.lexeme) :: errors, symbolTable, symbolTree


                | _ -> errors, symbolTable, symbolTree
            | VarElementList ->
                // Single( ManyOf [ DataMember; FunctionCall ] )
                let errorsAndTreeAndTreeTriples = List.map (makeSymbolTableAndTree [] symbolTable) children
                let trees = List.map trd errorsAndTreeAndTreeTriples

                let lastChildTree = List.tryLast trees
                let newSymbolTree = lastChildTree <?> (fun t -> symbolTree <<<< { root with symbolEntry = t.root.symbolEntry })
                (List.flatMap (fun (a, b, c) -> a) errorsAndTreeAndTreeTriples) @ errors, symbolTable, newSymbolTree @? symbolTree
            | _ ->
                let errorsAndTreeAndTreeTriples = List.map (makeSymbolTableAndTree [] symbolTable) children
                let trees = List.map trd errorsAndTreeAndTreeTriples

                (List.flatMap (fun (a, b, c) -> a) errorsAndTreeAndTreeTriples) @ errors, symbolTable, Tree.create symbolTree.root trees

        let visit syntaxTree =
            let symbolTable, symbolTree = SymbolTable.makeSymbolTableAndTree syntaxTree
            makeSymbolTableAndTree [] symbolTable symbolTree

    [<RequireQualifiedAccess>]
    module TypeCheckVisitor =
        let rec assignStat (accumulatedErrors, parentType) tree =
            let a = Ast.typeAt 0 tree
            let b = Ast.typeAt 1 tree

            let errors = []

            match a, b with
            | Some lhs, Some rhs ->
                if lhs.dimensionality <> rhs.dimensionality then
                    (ArrayDimensionMismatch(Ast.firstToken tree, tree.children.[0].root.symbolEntry @! "", rhs) :: errors, None)
                else
                    let isTypeMismatched =
                        match lhs, rhs with
                        | Integer _, Integer _
                        | Float _, Float _
                        | Float _, Integer _ -> false
                        | SymbolType.Class(lhsClassName, _), SymbolType.Class(rhsClassName, _) -> lhsClassName <> rhsClassName
                        | _ -> true
                    if isTypeMismatched
                    then (TypeMismatch(Ast.firstToken tree, lhs, rhs) :: errors, Some lhs)
                    else (errors, Some lhs)
            | _, _ -> (errors, None)


        //        let rec visit2 errors tree =
        //            errors @ (match tree.root.syntaxElement.syntaxKind with
        //                      | AssignStat -> assignStat [] tree
        //                      | _ -> [])
        //                     @ List.flatMap (visit2 []) tree.children

        and visit (accumulatedErrors: SemanticError list, parentType: SymbolType option) tree: SemanticError list * SymbolType option =
            let dispatchMap = Map.ofList [ (AssignStat, assignStat) ]
            let keyResolver tree = tree.root.syntaxElement.syntaxKind

            Tree.visit (accumulatedErrors, parentType) dispatchMap keyResolver tree

    let check tree =
        let symTa1, symTr1 = SymbolTable.makeSymbolTableAndTree tree
        let symErs, symTa, symTr = SymbolTableVisitor.visit tree
        let typErs, _ = TypeCheckVisitor.visit ([], None) symTr

        tree
        |> SymbolTableVisitor.visit
        |||> (fun symbolErrors symbolTable symbolTree -> symbolTable, symbolTree, symbolErrors @ fst (TypeCheckVisitor.visit ([], None) symbolTree))
        |||> (fun symbolTable symbolTree typeErrors -> typeErrors, symbolTable, symbolTree)
