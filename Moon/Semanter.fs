namespace Moon

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

    | ArrayDimensionMismatch of Token * SymbolType * SymbolType // Different dimensions between decl/def
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

type SemanterSymbolTableContext =
    { globalSymbolTable: SymbolTable
      localSymbolTable: SymbolTable }

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

    module Ast =
        let typeAt index (symbolTree: Tree<SymbolElement>) =
            match (List.tryItem index symbolTree.children) with
            | Some tree -> tree.root.symbolType
            | None -> None

        let rec firstToken symbolTree =
            match symbolTree.children with
            | [] ->
                symbolTree.root.syntaxElement.token ||| failwith "Semanter.firstToken: Tried to `Option.get None`"
            | x :: xs -> firstToken x

    module TypeCheckVisitor =
        let assignStat tree errors =
            match Ast.typeAt 0 tree, Ast.typeAt 1 tree with
            | Some lhs, Some rhs ->
                if SymbolTable.dimensions lhs <> SymbolTable.dimensions rhs
                then TypeMismatch(Ast.firstToken tree, lhs, rhs) :: errors
                else errors
            | _, _ -> errors
