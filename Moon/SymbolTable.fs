namespace Moon

type SymbolKind =
    | Variable
    | Parameter
    | Function
    | Class
    | ProgKind

[<StructuredFormatDisplay("{show}")>]
type SymbolType =
    | VariableType of Token * int list option // type, dim[]
    | FreeFunctionType of Token * SymbolType list // returnType, paramType[]
    | ClassFunctionType of Token * SymbolType list * Token // returnType, paramType[], classType
    | ClassType of Token list // inheritance chain
    | Nil
    member x.show =
        match x with
        | VariableType(token, dimensions) ->
            show token.lexeme + match dimensions with
                                | Some [] -> "[]"
                                | Some array -> show array
                                | None -> ""
        | FreeFunctionType(token, symbolTypes) ->
            "(" + String.concat ", " (List.map show symbolTypes) + "): " + show token.lexeme
        | ClassFunctionType(token, symbolTypes, idToken) ->
            "(" + String.concat ", " (List.map show symbolTypes) + "): " + show token.lexeme + " [" + show idToken.lexeme + "]"
        | ClassType tokens ->
            if tokens = []
            then ""
            else String.concat ", " (List.map (fun (t: Token) -> t.lexeme) tokens)
        | Nil -> ""

[<StructuredFormatDisplay("{show}")>]
type SymbolTable =
    { symbolName: Token
      symbolKind: SymbolKind
      symbolType: SymbolType
      entries: SymbolTable list
      tree: Tree<SyntaxElement> }
    member x.show =
        match x.symbolType with
        | ClassFunctionType(token, symbolTypes, classToken) ->
            show x.symbolKind + " " + show classToken.lexeme + "." + x.symbolName.lexeme + "("
            + String.concat ", " (List.map (fun (st: SymbolType) -> st.show) symbolTypes) + "): " + show token.lexeme
        | _ ->
            show x.symbolKind + " " + x.symbolName.lexeme + if show x.symbolType = "" then "" else ": " + show x.symbolType

[<StructuredFormatDisplay("{show}")>]
type SymbolElement =
    { syntaxElement: SyntaxElement
      symbolEntry: SymbolTable option }

    member x.show =
        if Option.isSome x.symbolEntry
        then show x.syntaxElement + ", " + show (Option.get x.symbolEntry)
        else show x.syntaxElement

    member x.symbolType = x.symbolEntry.map (fun se -> se.symbolType)

[<RequireQualifiedAccess>]
module SymbolTable =
    let dimensions symbolType =
        match symbolType with
        | VariableType(_, dimensionsMaybe) -> dimensionsMaybe ||| []
        | _ -> failwith "Tried to get dimensions of non VariableType SymbolType"

    let symbolTypeWithId (symbolTable: SymbolTable) (idTokenMaybe: Token option) =
        match idTokenMaybe with
        | Some idToken ->
            let finder (nodalSymbolTable: SymbolTable) =
                match nodalSymbolTable.symbolKind with
                | SymbolKind.Variable
                | SymbolKind.Parameter -> nodalSymbolTable.symbolName.lexeme = idToken.lexeme
                | _ -> false

            let mapToSymbolType st = st.symbolType
            List.tryFind finder symbolTable.entries |> Option.map mapToSymbolType
        | None -> failwith "ABORT: Expected Some idTokenMaybe, but was None"

    let isVariable symbolTableEntry =
        match symbolTableEntry with
        | Variable -> true
        | _ -> false

    let tokenFromSyntaxIdNode (node: Tree<SyntaxElement>) =
        let syntaxId = node.root
        Option.get syntaxId.token

    let nameFromSyntaxIdNode (node: Tree<SyntaxElement>) =
        (tokenFromSyntaxIdNode node).lexeme

    let namesFromSyntaxInheritListNode (node: Tree<SyntaxElement>) =
        List.map nameFromSyntaxIdNode node.children

    let tokensFromSyntaxInheritListNode (node: Tree<SyntaxElement>) =
        List.map tokenFromSyntaxIdNode node.children

    let tokenFromSyntaxTypeNode (node: Tree<SyntaxElement>) =
        let syntaxType = node.root
        (Option.get syntaxType.token)

    let intListFromSyntaxDimListNode (nodeMaybe: Tree<SyntaxElement> option) =
        match nodeMaybe with
        | Some node ->
            let syntaxNumNodes = node.children
            List.map (int << nameFromSyntaxIdNode) syntaxNumNodes |> Some
        | None -> None

    let rec makeSymbolEntry (syntaxElementNode: Tree<SyntaxElement>): SymbolTable option * Tree<SymbolElement> =
        let syntaxElement = syntaxElementNode.root

        let symbolVarDeclEntriesFromSyntaxFParamListNode (node: Tree<SyntaxElement>) =
            let symbolEntryAndTreePairs = List.map makeSymbolEntry node.children
            List.map2 (fun se t -> (Option.get se), t) (List.map fst symbolEntryAndTreePairs) (List.map snd symbolEntryAndTreePairs)

        let symbolTypesFromSyntaxFParamListNode (node: Tree<SyntaxElement>) =
            let symbolEntryAndTreePairs = List.map makeSymbolEntry node.children
            List.map2 (fun se t -> (Option.get se).symbolType, t) (List.map fst symbolEntryAndTreePairs) (List.map snd symbolEntryAndTreePairs)

        match syntaxElement.syntaxKind with
        | VarDecl ->
            let syntaxTypeNode = syntaxElementNode.children.[0]
            let syntaxIdNode = syntaxElementNode.children.[1]
            let syntaxDimListNodeMaybe = List.tryItem 2 syntaxElementNode.children

            let symbolEntry =
                { symbolName = tokenFromSyntaxIdNode syntaxIdNode
                  symbolKind = Variable
                  symbolType = VariableType(tokenFromSyntaxTypeNode syntaxTypeNode, intListFromSyntaxDimListNode syntaxDimListNodeMaybe)
                  entries = []
                  tree = syntaxElementNode }

            Some symbolEntry,
            { root =
                  { syntaxElement = syntaxElement
                    symbolEntry = Some symbolEntry }
              children =
                  List.flatten
                      [ syntaxElementNode.children
                        |> List.map makeSymbolEntry
                        |> List.map snd ] }
        | FParam ->
            let syntaxTypeNode = syntaxElementNode.children.[0]
            let syntaxIdNode = syntaxElementNode.children.[1]
            let syntaxDimListNodeMaybe = List.tryItem 2 syntaxElementNode.children

            let symbolEntry =
                { symbolName = tokenFromSyntaxIdNode syntaxIdNode
                  symbolKind = Parameter
                  symbolType = VariableType(tokenFromSyntaxTypeNode syntaxTypeNode, intListFromSyntaxDimListNode syntaxDimListNodeMaybe)
                  entries = []
                  tree = syntaxElementNode }
            Some symbolEntry,
            { root =
                  { syntaxElement = syntaxElement
                    symbolEntry = Some symbolEntry }
              children =
                  List.flatten
                      [ syntaxElementNode.children
                        |> List.map makeSymbolEntry
                        |> List.map snd ] }
        | FuncDecl ->
            let syntaxIdNode = syntaxElementNode.children.[0]
            let syntaxFParamListNode = syntaxElementNode.children.[1]
            let syntaxReturnTypeNode = syntaxElementNode.children.[2]

            let paramFParamEntryAndTreePairs = symbolVarDeclEntriesFromSyntaxFParamListNode syntaxFParamListNode
            let paramFParamEntries = List.map fst paramFParamEntryAndTreePairs
            //let paramFParamTrees = List.map snd paramFParamEntryAndTreePairs

            let paramFParamTypeAndTreePairs = symbolTypesFromSyntaxFParamListNode syntaxFParamListNode
            let paramFParamTypes = List.map fst paramFParamTypeAndTreePairs
            //let paramFParamTrees = List.map snd paramFParamEntryAndTreePairs

            let symbolEntry =
                { symbolName = tokenFromSyntaxIdNode syntaxIdNode
                  symbolKind = Function
                  symbolType = FreeFunctionType(tokenFromSyntaxTypeNode syntaxReturnTypeNode, paramFParamTypes)
                  entries = paramFParamEntries
                  tree = syntaxElementNode }

            Some symbolEntry,
            { root =
                  { syntaxElement = syntaxElement
                    symbolEntry = Some symbolEntry }
              children =
                  List.flatten
                      [ syntaxElementNode.children
                        |> List.map makeSymbolEntry
                        |> List.map snd ] }
        | ClassDecl ->
            let syntaxIdNode = syntaxElementNode.children.[0]
            let syntaxInheritListNode = syntaxElementNode.children.[1]
            let syntaxMemberDeclListNode = syntaxElementNode.children.[2]

            let mapFuncDeclEntry entry =
                match entry.symbolType with
                | FreeFunctionType(token, symbolTypes) ->
                    { symbolName = entry.symbolName
                      symbolKind = entry.symbolKind
                      symbolType = ClassFunctionType(token, symbolTypes, (tokenFromSyntaxTypeNode syntaxIdNode))
                      entries = entry.entries
                      tree = syntaxElementNode }
                | _ -> entry

            let symbolEntryAndTreePairs = List.map makeSymbolEntry syntaxMemberDeclListNode.children
            let symbolEntries = List.map fst symbolEntryAndTreePairs
            //let symbolTrees = List.map snd symbolEntryAndTreePairs

            let symbolEntry =
                { symbolName = tokenFromSyntaxIdNode syntaxIdNode
                  symbolKind = Class
                  symbolType = ClassType(tokensFromSyntaxInheritListNode syntaxInheritListNode)
                  entries =
                      symbolEntries
                      |> List.choose id
                      |> List.map mapFuncDeclEntry
                  tree = syntaxElementNode }

            Some symbolEntry,
            { root =
                  { syntaxElement = syntaxElement
                    symbolEntry = Some symbolEntry }
              children =
                  List.flatten
                      [ syntaxElementNode.children
                        |> List.map makeSymbolEntry
                        |> List.map snd ] }
        | FuncDef ->
            let syntaxTypeOrEpsilonNode = syntaxElementNode.children.[0]
            let syntaxIdNode = syntaxElementNode.children.[1]
            let syntaxFParamListNode = syntaxElementNode.children.[2]
            let syntaxReturnTypeNode = syntaxElementNode.children.[3]
            let syntaxFuncBodyNode = syntaxElementNode.children.[4]

            let paramFParamEntryAndTreePairs = symbolVarDeclEntriesFromSyntaxFParamListNode syntaxFParamListNode
            let paramFParamEntries = List.map fst paramFParamEntryAndTreePairs
            //let paramFParamTrees =  List.map snd paramFParamEntryAndTreePairs

            let funcBodyEntryAndTreePairs = (List.map makeSymbolEntry syntaxFuncBodyNode.children)
            let funcBodyEntries = List.choose (id << fst) funcBodyEntryAndTreePairs
            //let funcBodyTrees = List.map snd funcBodyEntryAndTreePairs

            match syntaxTypeOrEpsilonNode.root.syntaxKind with
            | SyntaxKind.Epsilon ->
                let symbolEntry =
                    { symbolName = tokenFromSyntaxIdNode syntaxIdNode
                      symbolKind = Function
                      symbolType =
                          FreeFunctionType
                              (tokenFromSyntaxTypeNode syntaxReturnTypeNode,
                               symbolTypesFromSyntaxFParamListNode syntaxFParamListNode |> List.map fst)
                      entries = paramFParamEntries @ funcBodyEntries
                      tree = syntaxElementNode }

                Some symbolEntry,
                { root =
                      { syntaxElement = syntaxElement
                        symbolEntry = Some symbolEntry }
                  children =
                      List.flatten
                          [ syntaxElementNode.children
                            |> List.map makeSymbolEntry
                            |> List.map snd ] }
            | Type ->
                let symbolEntry =
                    { symbolName = tokenFromSyntaxIdNode syntaxIdNode
                      symbolKind = Function
                      symbolType =
                          ClassFunctionType
                              (tokenFromSyntaxTypeNode syntaxReturnTypeNode,
                               symbolTypesFromSyntaxFParamListNode syntaxFParamListNode |> List.map fst,
                               tokenFromSyntaxTypeNode syntaxTypeOrEpsilonNode)
                      entries = paramFParamEntries @ funcBodyEntries
                      tree = syntaxElementNode }

                Some symbolEntry,
                { root =
                      { syntaxElement = syntaxElement
                        symbolEntry = Some symbolEntry }
                  children =
                      List.flatten
                          [ syntaxElementNode.children
                            |> List.map makeSymbolEntry
                            |> List.map snd ] }
            | syntaxKind -> failwith ("ABORT: Should of been a `Epsilon` or `Type` SyntaxKind, but was a `" + show syntaxKind + "`")
        | _ ->
            None,
            { root =
                  { syntaxElement = syntaxElement
                    symbolEntry = None }
              children =
                  List.flatten
                      [ syntaxElementNode.children
                        |> List.map makeSymbolEntry
                        |> List.map snd ] }

    let makeSymbolTableAndTree (tree: Tree<SyntaxElement>) =
        let syntaxElementNode = tree

        match tree.root.syntaxKind with
        | Prog ->
            let syntaxClassDeclListNode = syntaxElementNode.children.[0]
            let syntaxFuncDefListNode = syntaxElementNode.children.[1]
            let syntaxMainFuncBodyNode = syntaxElementNode.children.[2]

            let symbolClassDeclEntryAndTreePairs = List.map makeSymbolEntry syntaxClassDeclListNode.children
            let symbolFuncDefEntryAndTreePairs = List.map makeSymbolEntry syntaxFuncDefListNode.children
            let symbolMainFuncBodyEntryAndTreePairs = List.map makeSymbolEntry syntaxMainFuncBodyNode.children

            let symbolClassDeclEntries = List.choose (id << fst) symbolClassDeclEntryAndTreePairs
            let symbolFuncDefEntries = List.choose (id << fst) symbolFuncDefEntryAndTreePairs
            let symbolMainFuncBodyEntries = List.choose (id << fst) symbolMainFuncBodyEntryAndTreePairs

            let symbolClassDeclListEntry =
                { symbolName =
                      { tokenType = TokenType.Class
                        location =
                            { line = 0
                              column = 0 } }
                  symbolKind = Class
                  symbolType = Nil
                  entries = symbolClassDeclEntries
                  tree = syntaxClassDeclListNode }

            let symbolClassDeclListTree =
                { root =
                      { syntaxElement = tree.children.[0].root
                        symbolEntry = Some symbolClassDeclListEntry }
                  children = List.map snd symbolClassDeclEntryAndTreePairs }

            let symbolFuncDefListEntry =
                { symbolName =
                      { tokenType = TokenType.Class
                        location =
                            { line = 0
                              column = 0 } }
                  symbolKind = Function
                  symbolType = Nil
                  entries = symbolFuncDefEntries
                  tree = syntaxFuncDefListNode }

            let symbolFuncDefListTree =
                { root =
                      { syntaxElement = tree.children.[0].root
                        symbolEntry = Some symbolFuncDefListEntry }
                  children = List.map snd symbolFuncDefEntryAndTreePairs }

            let symbolMainFuncBodyEntry =
                { symbolName =
                      { tokenType = TokenType.Main
                        location =
                            { line = 0
                              column = 0 } }
                  symbolKind = Function
                  symbolType = Nil
                  entries = symbolMainFuncBodyEntries
                  tree = syntaxMainFuncBodyNode }

            let symbolMainFuncBodyTree =
                { root =
                      { syntaxElement = tree.children.[0].root
                        symbolEntry = Some symbolMainFuncBodyEntry }
                  children = List.map snd symbolMainFuncBodyEntryAndTreePairs }

            let symbolProgEntry =
                { symbolName =
                      { tokenType = TokenType.Id "Prog"
                        location =
                            { line = 0
                              column = 0 } }
                  symbolKind = ProgKind
                  symbolType = Nil
                  entries = symbolClassDeclEntries @ symbolFuncDefEntries @ [ symbolMainFuncBodyEntry ]
                  tree = tree }

            let symbolTree =
                { root =
                      { syntaxElement =
                            { syntaxKind = Prog
                              token = None }
                        symbolEntry = Some symbolProgEntry }
                  children = [ symbolClassDeclListTree; symbolFuncDefListTree; symbolMainFuncBodyTree ] }

            symbolProgEntry, symbolTree
        | _ -> failwith "ABORT: Tree tip root is not a Prog syntax element"

    let prefMid =
        seq {
            yield "├─"
            while true do
                yield "│ "
        }

    let prefEnd =
        seq {
            yield "└─"
            while true do
                yield "  "
        }

    let prefNone =
        seq {
            while true do
                yield ""
        }

    let rec drawSymbolEntry (entry: SymbolTable) (pre: seq<string>) =
        seq {
            yield (Seq.head pre) + show entry
            if entry.entries <> [] then
                let preRest = Seq.skip 1 pre
                let last = List.last entry.entries
                for e in entry.entries do
                    if e = last
                    then yield! drawSymbolEntry e (Seq.map2 (+) preRest prefEnd)
                    else yield! drawSymbolEntry e (Seq.map2 (+) preRest prefMid)
        }

    let drawSymbolTable (symbolTable: SymbolTable) =
        drawSymbolEntry symbolTable prefNone
        |> List.ofSeq
        |> String.concat "\n"
