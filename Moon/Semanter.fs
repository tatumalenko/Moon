namespace Moon

type SemanticError =
    | UnknownIdentifier
    | TypeMismatch
    | DeclaredButNotDefined
    | DefinedButNotDeclared
    | AlreadyDeclaredIdentifier
    | CircularInheritance
    | ShadowedInheritance // Warning only
    | UnknownError

type SemanterSymbolTableContext =
    { globalSymbolTable: SymbolTable
      localSymbolTable: SymbolTable }

//type NodeType =
//    {
//
//    }

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

    let idSymbolType (context: SemanterSymbolTableContext) (tree: Tree<SyntaxElement>) =
        let syntaxElement = tree.root
        let tokenMaybe = syntaxElement.token
        let localIdMaybe = SymbolTable.symbolTypeWithId context.localSymbolTable tokenMaybe
        let globalIdMaybe = SymbolTable.symbolTypeWithId context.globalSymbolTable tokenMaybe
        match localIdMaybe, globalIdMaybe with
        | Some localId, Some _
        | Some localId, None ->
            Ok localId
        | None, Some globalId ->
            Ok globalId
        | None, None ->
            Error(UnknownIdentifier, "Unknown identifier: " + show tokenMaybe)

    let rec typeOf (context: SemanterSymbolTableContext) (tree: Tree<SyntaxElement>) =
        let syntaxElement = tree.root
        let tokenMaybe = syntaxElement.token
        match syntaxElement.syntaxKind with
        | Data
        | SyntaxKind.Epsilon
        | Idi
        | RelOp
        | Type
        | EndProgram -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | Id ->
            let localIdMaybe = SymbolTable.symbolTypeWithId context.localSymbolTable tokenMaybe
            let globalIdMaybe = SymbolTable.symbolTypeWithId context.globalSymbolTable tokenMaybe
            match localIdMaybe, globalIdMaybe with
            | Some localId, Some _ ->
                Ok(show localId)
            | Some localId, None ->
                Ok(show localId)
            | None, Some globalId ->
                Ok(show globalId)
            | None, None ->
                Error(UnknownIdentifier, "Unknown identifier: " + show tokenMaybe)
        | Num ->
            match tokenMaybe with
            | Some integerLiteralToken when integerLiteralToken.tokenType = IntegerLiteral integerLiteralToken.lexeme ->
                Ok "integer"
            | Some floatLiteralToken when floatLiteralToken.tokenType = FloatLiteral floatLiteralToken.lexeme ->
                Ok "float"
            | Some otherTypeToken -> Error(UnknownError, "SyntaxKind.Num is neither an integer or float but instead " + show otherTypeToken)
            | None -> Error(UnknownError, "SyntaxKind.Num.token is None")
        | ClassDeclList -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | FuncDefList -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | Prog -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | MemberDeclList -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | ClassDecl -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | FuncDecl -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | FuncDef -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | MainFuncBody -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | FuncBody -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | StatBlock -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | VarDecl -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | DimList -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | AssignStat -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | IfStat -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | WhileStat -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | ReadStat -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | WriteStat -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | ReturnStat -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | IndexList ->
            let arithExprNodes = tree.children
            let arithExprTypes = List.map (typeOf context) arithExprNodes
            Ok "IndexList"
        | RelExpr -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | AddOp ->
            let arithExprNodeMaybe = List.tryItem 0 tree.children
            let termNodeMaybe = List.tryItem 1 tree.children
            match arithExprNodeMaybe, termNodeMaybe with
            | Some arithExprNode, Some termNode ->
                let arithExprNodeTypeResult = typeOf context arithExprNode
                let termNodeTypeResult = typeOf context termNode
                match arithExprNodeTypeResult, termNodeTypeResult with
                | Ok arithExprNodeType, Ok termNodeType ->
                    if arithExprNodeType <> termNodeType
                    then Error(TypeMismatch, "AddOp type mismatch between: " + show arithExprNode.root + " and " + show termNode.root)
                    else Ok arithExprNodeType
                | Error semanticError, _ -> Error semanticError
                | _, Error semanticError -> Error semanticError
            | Some arithExprNode, None -> Error(UnknownError, "AddOp has no item at index 1 (expected term)")
            | None, Some termNode -> Error(UnknownError, "AddOp has no item at index 0 (expected arithExpr)")
            | None, None -> Error(UnknownError, "AddOp has no item at index 0 and 1 (expected arithExpr and term)")
        | MultOp -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | Not -> Ok "boolean"
        | Sign -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | VarElementList ->
            let dataMemberOrFunctionCallNodeMaybe = List.tryItem 0 tree.children
            match dataMemberOrFunctionCallNodeMaybe with
            | Some dataMemberOrFunctionCallNode -> typeOf context dataMemberOrFunctionCallNode
            | None -> Error(UnknownError, "VarElementList has no item at index 0 (expected DataMember or FunctionCall node")
        | DataMember ->
            let idNodeMaybe = List.tryItem 0 tree.children
            let indexListNodeMaybe = List.tryItem 1 tree.children

            match idNodeMaybe, indexListNodeMaybe with
            | Some idNode, Some indexListNode ->
                let idSymbolTypeResult = idSymbolType context idNode
                let indexListTypeResults = List.map (typeOf context) indexListNode.children
                let semanticErrors = semanticErrors indexListTypeResults

                match idSymbolTypeResult, semanticErrors with
                | Ok idSymbolType, [] ->
                    let idSymbolTypeStr = show idSymbolType
                    let idSymbolTypeDimListCount = squareBracketCount idSymbolTypeStr

                    let indexListTypeStrList =
                        List.map (fun result ->
                            match result with
                            | Ok ok -> ok
                            | Error _ -> failwith "DataMember unwrap of result failed (expected Ok, found Error)") indexListTypeResults
                    if (idSymbolTypeDimListCount <> List.length indexListTypeStrList) && (List.length indexListTypeStrList = 0) then
                        Ok idSymbolTypeStr
                    else if (idSymbolTypeDimListCount <> List.length indexListTypeStrList) then
                        Error
                            (TypeMismatch,
                             "Array dimension mismatch, expected " + show idSymbolTypeDimListCount + ", but detected "
                             + show (List.length indexListTypeStrList))
                    else if (not (List.forall (fun s -> s = "integer") indexListTypeStrList)) then
                        Error(TypeMismatch, "IndexList list contains non-integer type: " + show indexListTypeStrList)
                    else if List.length indexListTypeStrList > 0 then
                        Ok(String.split [ "[" ] idSymbolTypeStr |> Seq.item 0)
                    else
                        Ok idSymbolTypeStr

                //                    let idSymbolTypeStr = match idSymbolType with
                //                        | VariableType(idTypeToken, idDimsMaybe) -> show idSymbolType
                //                        | FreeFunctionType(idTypeToken, paramTypes) -> Error(UnknownError, "DataMember has no item at index 1 (expected IndexList)") // returnType, paramType[]
                //                        | ClassFunctionType(idTypeToken, paramTypes, classType) -> Error(UnknownError, "DataMember has no item at index 1 (expected IndexList)") // returnType, paramType[], classType
                //                        | ClassType(inheritTypes) -> Error(UnknownError, "DataMember has no item at index 1 (expected IndexList)") // inheritance chain
                //                        | Nil -> Error(UnknownError, "DataMember has no item at index 1 (expected IndexList)")
                | Error semanticError, _ -> Error semanticError
                | Ok _, x :: _ -> x
            | Some idNode, None -> Error(UnknownError, "DataMember has no item at index 1 (expected IndexList)")
            | None, Some indexListNode -> Error(UnknownError, "DataMember has no item at index 0 (expected Id")
            | None, None -> Error(UnknownError, "DataMember has no item at index 0 and 1 (expected Id or IndexList")

        //            match tree.children with
        //            | [] ->
        //                Error(UnknownIdentifier, "No DataMember children found")
        //            | idNode :: otherNodes ->
        //                let idTypeMaybe = SymbolTable.symbolTypeWithId context.localSymbolTable idNode.root.token
        //                match idTypeMaybe with
        //                | Some idType ->
        //                    let idTypeStr, isArray =
        //                        match idType with
        //                        | VariableType(idTypeToken, idDimsMaybe) -> idTypeToken.lexeme, Option.isSome idDimsMaybe
        //                        | FreeFunctionType(idTypeToken, paramTypes) -> idTypeToken.lexeme, false // returnType, paramType[]
        //                        | ClassFunctionType(idTypeToken, paramTypes, classType) -> idTypeToken.lexeme, false // returnType, paramType[], classType
        //                        | ClassType(inheritTypes) -> "", false // inheritance chain
        //                        | Nil -> "", false
        //                    match otherNodes with
        //                    | [] ->
        //                        Ok idTypeStr
        //                    | indexListNode :: [] ->
        //                        match indexListNode.children with
        //                        | [] ->
        //                            if isArray then Ok idTypeStr else Ok idTypeStr
        //                        | indexListNodeChildren ->
        //                            if isArray
        //                            then Ok idTypeStr
        //                            else Error(TypeMismatch, "Trying to index a non-array type")
        //                    | _ -> Error(UnknownError, "DataMember.children has a count greater than 2")
        //                | None -> Error(UnknownIdentifier, "DataMember identifier is not in local symbol table")
        | FunctionCall ->
            let idNodeMaybe = List.tryItem 0 tree.children
            let aParamListNodeMaybe = List.tryItem 1 tree.children
            match idNodeMaybe, aParamListNodeMaybe with
            | Some idNode, Some aParamListNode ->
                let idSymbolTypeResult = idSymbolType context idNode

                match idSymbolTypeResult with
                | Ok idSymbolType ->
                    let aParamListNodeTypeResults = List.map (typeOf context) aParamListNode.children

                    let isSemanticError nodeTypeResult =
                        match nodeTypeResult with
                        | Ok _ -> false
                        | Error _ -> true

                    match List.filter isSemanticError aParamListNodeTypeResults with
                    | [] ->
                        let mapToOkValue result =
                            match result with
                            | Ok ok -> ok
                            | Error _ -> failwith "Tried to map result array to Ok case, but was Error"

                        let aParamListNodeTypes = List.map mapToOkValue aParamListNodeTypeResults
                        match idSymbolType with
                        | VariableType(idTypeToken, idDimsMaybe) -> Error(UnknownError, "FunctionCall idNode was VariableType (expected FunctionType")
                        | FreeFunctionType(idReturnTypeToken, idParamTypes) ->
                            let a = idParamTypes
                            let b = aParamListNodeTypes
                            if false
                            then Error(TypeMismatch, "FunctionCall param list does not match declared function param list")
                            else Ok idReturnTypeToken.lexeme
                        | ClassFunctionType(idReturnTypeToken, idParamTypes, idClassType) ->
                            let a = idParamTypes
                            let b = aParamListNodeTypes
                            if false
                            then Error(TypeMismatch, "FunctionCall param list does not match declared function param list")
                            else Ok idReturnTypeToken.lexeme
                        | ClassType(idTokenInheritTypes) -> failwith ""
                        | Nil -> failwith ""
                    | aParamListNodeTypeSemanticErrors -> aParamListNodeTypeSemanticErrors.[0]


                | Error semanticError -> Error semanticError
            | Some _, None -> Error(UnknownError, "FunctionCall has no item at index 1 (expected AParamList)")
            | None, Some _ -> Error(UnknownError, "FunctionCall has no item at index 0 (expected Id)")
            | None, None -> Error(UnknownError, "FunctionCall has no item at index 0 and 1 (expected Id and AParamList)")
        | InheritList -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | FParam -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | FParamList -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")
        | AParamList -> Error(UnknownError, "typeOf this SyntaxKind is not implemented")


    let isSameType (context: SemanterSymbolTableContext) (tree: Tree<SyntaxElement>) =
        ()

    let checkAssignStat (context: SemanterSymbolTableContext) (tree: Tree<SyntaxElement>) =
        match typeOf context tree.children.[0], typeOf context tree.children.[1] with
        | Ok idNodeTypeResult, Ok indexListNodeTypeResult ->
            if idNodeTypeResult <> indexListNodeTypeResult
            then [ TypeMismatch ]
            else []
        | Error semanticError, _ -> [ fst semanticError ]
        | _, Error semanticError -> [ fst semanticError ]
    //        if typeOf context tree.children.[0] <> typeOf context tree.children.[1] then
    //            let a = typeOf context tree.children.[0]
    //            let b = typeOf context tree.children.[1]
    //            []
    //        else
    //            let a = typeOf context tree.children.[0]
    //            let b = typeOf context tree.children.[1]
    //            []

    //        let mutable errors = []
    //
    //        let syntaxElement = tree.root
    //
    //        let lhsDataMemberNode = tree.children.[0].children.[0]
    //        let lhsIdNode = lhsDataMemberNode.children.[0]
    //        let lhsIndexListNode = lhsDataMemberNode.children.[1]
    //        let lhsTypeMaybe = SymbolTable.symbolTypeWithId context.localSymbolTable lhsIdNode.root.token
    //
    //        match lhsTypeMaybe with
    //        | Some(VariableType(lhsTypeToken, lhsTypeDimListMaybe)) ->
    //            let rhsExprNode = tree.children.[1]
    //
    //            match rhsExprNode.root.syntaxKind with
    //            | RelExpr -> []
    //            | AddOp -> []
    //            | MultOp -> []
    //            | Num ->
    //                // 1. Use SymbolTable to ensure lhs has same type (either integer or float)
    //                // 2. Use SymbolTable to check if lhs is array of non-array
    //                // 3. Use lhsNode to check if indexing is used (i.e. something inside lhsIndexListNode)
    //                // 4. If lhs is array type AND lhsIndexListNode is None, then TypeMismatch
    //                // 4. If lhs is array type AND lhsIndexListNode is Some, then OK
    //                // 5. Else if lhs is non-array type AND lhsIndexListNode is Some, then OK
    //                // 6. Else if lhs is non-array type AND lhsIndexListNode is Some, then
    //
    //                let rhsNumNode = tree.children.[1]
    //                let rhsNumMaybe = rhsNumNode.root.token
    //
    //                match lhsTypeDimListMaybe with
    //                | Some lhsTypeDimList ->
    //                    match lhsIndexListNode.children with
    //                    | [] ->
    //                        // Trying to assign num to array, i.e. TypeMismatch
    //                        TypeMismatch :: errors
    //                    | _ ->
    //                        // Check lhsIndexList type
    //                        // let lhsIndexListType = typeFromIndexListNode lhsIndexListNode
    //                        match rhsNumMaybe with
    //                        | Some rhsIntegerLiteral when rhsIntegerLiteral.tokenType = IntegerLiteral rhsIntegerLiteral.lexeme ->
    //                            if lhsTypeToken.lexeme <> "integer" then TypeMismatch :: errors else errors
    //                        | Some rhsFloatLiteral when rhsFloatLiteral.tokenType = FloatLiteral rhsFloatLiteral.lexeme ->
    //                            if lhsTypeToken.lexeme <> "float" then TypeMismatch :: errors else errors
    //                        | _ -> errors
    //                | _ ->
    //                    match rhsNumMaybe with
    //                    | Some rhsIntegerLiteral when rhsIntegerLiteral.tokenType = IntegerLiteral rhsIntegerLiteral.lexeme ->
    //                        if lhsTypeToken.lexeme <> "integer" then TypeMismatch :: errors else errors
    //                    | Some rhsFloatLiteral when rhsFloatLiteral.tokenType = FloatLiteral rhsFloatLiteral.lexeme ->
    //                        if lhsTypeToken.lexeme <> "float" then TypeMismatch :: errors else errors
    //                    | _ -> []
    //                if
    //            | Not -> []
    //            | Sign -> []
    //            | VarElementList ->
    //                let rhsDataMemberNode = tree.children.[1].children.[0]
    //                let rhsIdNode = rhsDataMemberNode.children.[0]
    //                let rhsIndexListNode = rhsDataMemberNode.children.[1]
    //                let rhsTypeMaybe = SymbolTable.symbolTypeWithId context.localSymbolTable rhsIdNode.root.token
    //
    //                errors <-
    //                    match rhsTypeMaybe with
    //                    | Some(VariableType(rhsTypeToken, rhsTypeDimList)) ->
    //
    //                        if rhsTypeToken.lexeme <> lhsTypeToken.lexeme
    //                        then TypeMismatch :: errors
    //                        else errors
    //                    | None -> UnknownIdentifier :: errors
    //                    | _ -> errors
    //
    //                errors
    //            | syntaxKind -> failwith ("ABORT: rhsExprNode should not contain " + show syntaxKind)
    //        | _ -> []

    let check (globalSymbolTable: SymbolTable) =
        let rec checkRec (semanticErrors: SemanticError list) (context: SemanterSymbolTableContext) (tree: Tree<SyntaxElement>) =
            match tree.root.syntaxKind with
            | SyntaxKind.MainFuncBody ->
                ()
            | _ -> ()

            semanticErrors @ (match tree.root.syntaxKind with
                              | Data
                              | SyntaxKind.Epsilon
                              | Id
                              | Idi
                              | Num
                              | RelOp
                              | Type
                              | EndProgram -> []
                              | ClassDeclList -> []
                              | FuncDefList -> []
                              | Prog -> []
                              | MemberDeclList -> []
                              | ClassDecl -> []
                              | FuncDecl -> []
                              | FuncDef -> []
                              | MainFuncBody -> []
                              | FuncBody -> []
                              | StatBlock -> []
                              | VarDecl -> []
                              | DimList -> []
                              | AssignStat -> checkAssignStat context tree
                              | IfStat -> []
                              | WhileStat -> []
                              | ReadStat -> []
                              | WriteStat -> []
                              | ReturnStat -> []
                              | IndexList -> []
                              | RelExpr -> []
                              | AddOp -> []
                              | MultOp -> []
                              | Not -> []
                              | Sign -> []
                              | VarElementList -> []
                              | DataMember -> []
                              | FunctionCall -> []
                              | InheritList -> []
                              | FParam -> []
                              | FParamList -> []
                              | AParamList -> [])
                             @ List.flatMap (checkRec [] context) tree.children

        let vv = (List.map (fun localSymbolTable -> localSymbolTable.tree) globalSymbolTable.entries)

        List.flatMap2 (checkRec [])
            (List.map (fun localSymbolTable ->
                { globalSymbolTable = globalSymbolTable
                  localSymbolTable = localSymbolTable }) globalSymbolTable.entries)
            (List.map (fun localSymbolTable -> localSymbolTable.tree) globalSymbolTable.entries)
