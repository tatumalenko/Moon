namespace Moon

[<StructuredFormatDisplay("{show}")>]
type SymbolType =
    | Integer of int list
    | Float of int list
    | Class of string * int list
    | Void
    | Nil

    member x.withoutDimensionality =
        match x with
        | Integer _ -> Integer []
        | Float _ -> Float []
        | Class(className, _) -> Class(className, [])
        | Void
        | Nil -> failwith "SymbolType.dimensions: Tried to get dimensions of non-Integer, Float, or Class SymbolType"

    member x.dimensions =
        match x with
        | Integer dimensions
        | Float dimensions
        | Class(_, dimensions) -> dimensions
        | Void
        | Nil -> failwith "SymbolType.dimensions: Tried to get dimensions of non-Integer, Float, or Class SymbolType"

    member x.className =
        match x with
        | Class(className, _) -> className
        | Integer _
        | Float _
        | Void
        | Nil -> failwith "SymbolType.className: Tried to get name of non-Class SymbolType DU"

    static member make (typeToken: Token) (dimensions: int list option) =
        match typeToken.lexeme with
        | "integer" -> (dimensions.map (fun xs -> Integer xs)) @! "SymbolType.make: `typeToken.lexeme` = 'integer' but `dimensions` given is None"
        | "float" -> (dimensions.map (fun xs -> Float xs)) @! "SymbolType.make: `typeToken.lexeme` = 'float' but `dimensions` given is None"
        | "void" -> Void
        | Int _ -> Integer []
        | Double _ -> Float []
        | className ->
            (dimensions.map (fun xs -> Class(className, xs)))
            @! ("SymbolType.make: `typeToken.lexeme` = '" + className + "' but `dimensions` given is None")

    member x.fakeToken =
        let fakeLexeme =
            match x with
            | Integer _ -> "integer"
            | Float _ -> "float"
            | Class(className, _) -> className
            | Void -> "void"
            | Nil -> ""
        { tokenType = TokenType.Id fakeLexeme
          location =
              { line = 0
                column = 0 } }

    member x.dimensionality =
        List.length x.dimensions

    member inline x.show =
        match x with
        | Integer dimensions -> "integer" + List.fold (fun s e -> s + "[" + show e + "]") "" dimensions
        | Float dimensions -> "float" + List.fold (fun s e -> s + "[" + show e + "]") "" dimensions
        | Class(className, dimensions) -> className + List.fold (fun s e -> s + "[" + show e + "]") "" dimensions
        | Void -> "void"
        | Nil -> ""

[<StructuredFormatDisplay("{show}")>]
type SymbolKind =
    | Variable of SymbolType
    | Parameter of SymbolType
    | FreeFunction of SymbolType * SymbolType list // returnType, paramType[]
    | MemberFunction of SymbolType * SymbolType list * SymbolType // returnType, paramType[], classType
    | Class of SymbolType list // superType[]
    | ProgKind
    | Nil

    member x.withNoDimensionality =
        match x with
        | Variable symbolType -> Variable symbolType.withoutDimensionality
        | Parameter symbolType -> Parameter symbolType.withoutDimensionality
        | FreeFunction (symbolType, _) -> FreeFunction (symbolType.withoutDimensionality, [])
        | MemberFunction (symbolType, _, superTypes) -> MemberFunction (symbolType.withoutDimensionality, [], superTypes)
        | Class _
        | ProgKind
        | Nil -> x

    member x.symbolType =
        match x with
        | Variable symbolType
        | Parameter symbolType -> Some symbolType
        | FreeFunction _ -> x.returnType
        | MemberFunction _ -> x.returnType
        | Class _ -> None
        | ProgKind -> None
        | Nil -> None

    member x.returnType =
        match x with
        | FreeFunction(symbolType, _) -> Some symbolType
        | MemberFunction(symbolType, _, _) -> Some symbolType
        | Variable _
        | Parameter _
        | Class _
        | ProgKind
        | Nil -> failwith "SymbolTable.returnType: Tried to access returnType of non-Function SymbolKind"

    member x.paramTypes =
        match x with
        | FreeFunction(_, symbolTypes) -> symbolTypes
        | MemberFunction(_, symbolTypes, _) -> symbolTypes
        | Variable _
        | Parameter _
        | Class _
        | ProgKind
        | Nil -> failwith "SymbolTable.paramTypes: Tried to access paramTypes of non-Function SymbolKind"

    member x.superTypes =
        match x with
        | Class superTypes -> superTypes
        | FreeFunction _
        | MemberFunction _
        | Variable _
        | Parameter _
        | ProgKind
        | Nil -> failwith "SymbolTable.returnType: Tried to access superTypes of non-Class SymbolKind"

    member inline x.show =
        match x with
        | Variable symbolType -> show symbolType
        | Parameter symbolType -> show symbolType
        | FreeFunction(returnType, paramTypes) -> "(" + String.concat ", " (List.map show paramTypes) + "): " + show returnType
        | MemberFunction(returnType, paramTypes, _) -> "(" + String.concat ", " (List.map show paramTypes) + "): " + show returnType
        | Class superTypes ->
            if superTypes = []
            then ""
            else ":" + String.concat ", " (List.map show superTypes)
        | ProgKind -> "Prog"
        | Nil -> ""


[<StructuredFormatDisplay("{show}")>]
type SymbolTable =
    { name: Token
      kind: SymbolKind
      entries: SymbolTable list
      tree: Tree<SyntaxElement>
      globalTree: Tree<SymbolElement> option }

    member x.symbolTree =
        x.globalTree @! "SymbolTable.symbolTree: Tried to get `globalTree` but was None"

    member x.withNoDimensionality =
        { x with kind = x.kind.withNoDimensionality }

    member x.symbolType =
        match x.kind with
        | Variable symbolType
        | Parameter symbolType -> Some symbolType
        | FreeFunction(_, _) -> x.returnType
        | MemberFunction(_, _, _) -> x.returnType
        | Class _ -> None
        | ProgKind -> None
        | Nil -> None

    member x.returnType =
        match x.kind with
        | FreeFunction(symbolType, _) -> Some symbolType
        | MemberFunction(symbolType, _, _) -> Some symbolType
        | Variable _
        | Parameter _
        | Class _
        | ProgKind
        | Nil -> failwith "SymbolTable.returnType: Tried to access returnType of non-Function SymbolKind"

    member x.paramTypes =
        match x.kind with
        | FreeFunction(_, symbolTypes) -> symbolTypes
        | MemberFunction(_, symbolTypes, _) -> symbolTypes
        | Variable _
        | Parameter _
        | Class _
        | ProgKind
        | Nil -> failwith "SymbolTable.paramTypes: Tried to access paramTypes of non-Function SymbolKind"

    member x.superTypes =
        match x.kind with
        | Class superTypes -> superTypes
        | FreeFunction _
        | MemberFunction _
        | Variable _
        | Parameter _
        | ProgKind
        | Nil -> failwith "SymbolTable.returnType: Tried to access superTypes of non-Class SymbolKind"

    member inline x.show =
        match x.kind with
        | Variable _ -> "Variable " + x.name.lexeme + ": " + show x.kind
        | Parameter _ -> "Parameter " + x.name.lexeme + ": " + show x.kind
        | FreeFunction(_, _) -> "Function " + x.name.lexeme + show x.kind
        | MemberFunction(_, _, classType) -> "Function " + show classType + "." + x.name.lexeme + show x.kind
        | Class _ -> "Class " + x.name.lexeme + show x.kind
        | ProgKind -> "Prog"
        | Nil -> x.name.lexeme



and [<StructuredFormatDisplay("{show}")>]
 SymbolElement =
    { syntaxElement: SyntaxElement
      symbolEntry: SymbolTable option }

    member x.syntaxToken = x.syntaxElement.token @! "SymbolElement.syntaxToken: Tried to get `syntaxElement.token` but was None"

    member x.symbolType = x.symbolEntry.map (fun it -> it.symbolType @! "SymbolElement.symbolType: Tried to get `symbolType` but was None")

    member inline x.show =
        show x.syntaxElement + (x.symbolEntry.map (fun it ->
                                    if show it <> "" then ", " + show it else "")
                                @? "")

[<RequireQualifiedAccess>]
module SymbolTable =
//    let rec findSymbolTables (localScopes: SymbolTable list) treeToFind currentTree =
//        let tables =
//            match currentTree.root.syntaxElement.syntaxKind with
//            | Prog -> (currentTree.root.symbolEntry @! "ProgKind has no symbolEntry") :: localScopes
//            | FuncDef -> (currentTree.root.symbolEntry @! "FuncDef has no symbolEntry") :: localScopes
//            | MainFuncBody -> (currentTree.root.symbolEntry @! "MainFuncBody has no symbolEntry") :: localScopes
//            | _ -> localScopes
//        if treeToFind = currentTree
//        then tables
//        else (List.flatMap (findSymbolTables [] treeToFind) currentTree.children) @ tables
//    let rec findSymbolTables (localScopes: SymbolTable list) treeToFind currentTree =
//        let tables =
//            match currentTree.root.syntaxElement.syntaxKind with
//            | Prog -> (currentTree.root.symbolEntry @! "ProgKind has no symbolEntry") :: []
//            | FuncDef -> (currentTree.root.symbolEntry @! "FuncDef has no symbolEntry") :: []
//            | MainFuncBody -> (currentTree.root.symbolEntry @! "MainFuncBody has no symbolEntry") :: []
//            | _ -> localScopes
//        if treeToFind = currentTree
//        then tables
//        else
//            let ts = (List.flatMap (findSymbolTables [] treeToFind) currentTree.children)
//            if List.length ts > 1 || List.length tables > 1
//            then printfn "hey"
//            else ()
//            ts @ tables
    let rec findSymbolTables (localScope: SymbolTable option) treeToFind currentTree =
        let localScope =
            match currentTree.root.syntaxElement.syntaxKind with
            | Prog -> (currentTree.root.symbolEntry)
            | FuncDef -> (currentTree.root.symbolEntry)
            | ClassDecl ->currentTree.root.symbolEntry
            | MainFuncBody -> (currentTree.root.symbolEntry)
            | _ -> localScope
        if treeToFind = currentTree
        then localScope
        else
            let localScopes = List.map (findSymbolTables localScope treeToFind) currentTree.children
            if List.length (List.choose id localScopes) > 1
            then printfn "hey"
            else ()
            List.choose id localScopes |> List.tryHead

    let rec findSymbolTable (treeToFind: Tree<SymbolElement>) (symbolTable: SymbolTable) =
        let localScope = findSymbolTables None treeToFind symbolTable.symbolTree
        localScope
        //List.tryItem 0 xs

    let rec map (mapper: _ -> _) (symbolTable: SymbolTable): SymbolTable =
        match symbolTable.entries with
        | [] -> mapper symbolTable
        | xs -> { mapper symbolTable with entries = List.map mapper xs }

    let tryFind (predicate: _ -> bool) (symbolTable: SymbolTable) =
        let rec f (x: SymbolTable option): SymbolTable option =
            match x.map (fun it -> it.entries), predicate x with
            | _, true -> x
            | None, _ -> None
            | Some xs, false -> List.tryHead (List.choose id (List.map (f << Some) xs))
        f (Some symbolTable)

    let tryFindClassTable (tree: Tree<SymbolElement>) (symbolTable: SymbolTable): SymbolTable option =
        let localScope = findSymbolTable tree symbolTable
        match localScope.map (fun it -> it.kind) with
        | Some (SymbolKind.MemberFunction(returnType, paramTypes, classType)) -> tryFind (fun st -> st.map (fun it -> it.name.lexeme = classType.className) @? false) symbolTable
        | _ -> None

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
            List.map (int << nameFromSyntaxIdNode) syntaxNumNodes
        | None -> []

    let rec makeSymbolEntry (syntaxElementNode: Tree<SyntaxElement>): SymbolTable option * Tree<SymbolElement> =
        let syntaxElement = syntaxElementNode.root

        let symbolVarDeclEntriesFromSyntaxFParamListNode (node: Tree<SyntaxElement>) =
            let symbolEntryAndTreePairs = List.map makeSymbolEntry node.children
            List.map2 (fun se t -> (Option.get se), t) (List.map fst symbolEntryAndTreePairs) (List.map snd symbolEntryAndTreePairs)

        let symbolTypesFromSyntaxFParamListNode (node: Tree<SyntaxElement>) =
            let symbolEntryAndTreePairs = List.map makeSymbolEntry node.children
            List.map2 (fun (se: SymbolTable option) t -> (Option.get se).symbolType, t) (List.map fst symbolEntryAndTreePairs)
                (List.map snd symbolEntryAndTreePairs)

        match syntaxElement.syntaxKind with
        | VarDecl ->
            let syntaxTypeNode = syntaxElementNode.children.[0]
            let syntaxIdNode = syntaxElementNode.children.[1]
            let syntaxDimListNodeMaybe = List.tryItem 2 syntaxElementNode.children

            let symbolEntry =
                { name = tokenFromSyntaxIdNode syntaxIdNode
                  kind =
                      Variable
                          (SymbolType.make (tokenFromSyntaxTypeNode syntaxTypeNode) (Some(intListFromSyntaxDimListNode syntaxDimListNodeMaybe)))
                  entries = []
                  tree = syntaxElementNode
                  globalTree = None }

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
                { name = tokenFromSyntaxIdNode syntaxIdNode
                  kind =
                      Parameter
                          (SymbolType.make (tokenFromSyntaxTypeNode syntaxTypeNode) (Some(intListFromSyntaxDimListNode syntaxDimListNodeMaybe)))
                  entries = []
                  tree = syntaxElementNode
                  globalTree = None }
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
                { name = tokenFromSyntaxIdNode syntaxIdNode
                  kind = FreeFunction(SymbolType.make (tokenFromSyntaxTypeNode syntaxReturnTypeNode) (Some []), List.choose id paramFParamTypes)
                  entries = paramFParamEntries
                  tree = syntaxElementNode
                  globalTree = None }

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
                match entry.kind with
                | FreeFunction(returnType, paramTypes) ->
                    { name = entry.name
                      kind = MemberFunction(returnType, paramTypes, SymbolType.Class(nameFromSyntaxIdNode syntaxIdNode, []))
                      entries = entry.entries
                      tree = syntaxElementNode
                      globalTree = None }
                | _ -> entry

            let symbolEntryAndTreePairs = List.map makeSymbolEntry syntaxMemberDeclListNode.children
            let symbolEntries = List.map fst symbolEntryAndTreePairs
            //let symbolTrees = List.map snd symbolEntryAndTreePairs

            let symbolEntry =
                { name = tokenFromSyntaxIdNode syntaxIdNode
                  kind =
                      Class
                          (tokensFromSyntaxInheritListNode syntaxInheritListNode
                           |> List.map (fun superTypeToken -> superTypeToken.lexeme)
                           |> List.map (fun superTypeName -> SymbolType.Class(superTypeName, [])))
                  entries =
                      symbolEntries
                      |> List.choose id
                      |> List.map mapFuncDeclEntry
                  tree = syntaxElementNode
                  globalTree = None }

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
                    { name = tokenFromSyntaxIdNode syntaxIdNode
                      kind =
                          FreeFunction
                              (SymbolType.make (tokenFromSyntaxTypeNode syntaxReturnTypeNode) (Some []),
                               symbolTypesFromSyntaxFParamListNode syntaxFParamListNode |> List.map (Option.get << fst))
                      entries = paramFParamEntries @ funcBodyEntries
                      tree = syntaxElementNode
                      globalTree = None }

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
                    { name = tokenFromSyntaxIdNode syntaxIdNode
                      kind =
                          MemberFunction
                              (SymbolType.make (tokenFromSyntaxTypeNode syntaxReturnTypeNode) (Some []),
                               symbolTypesFromSyntaxFParamListNode syntaxFParamListNode |> List.map (Option.get << fst),
                               SymbolType.make (tokenFromSyntaxTypeNode syntaxTypeOrEpsilonNode) (Some []))
                      entries = paramFParamEntries @ funcBodyEntries
                      tree = syntaxElementNode
                      globalTree = None }

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
        | Num ->
            let symbolEntry =
                { name = tokenFromSyntaxIdNode syntaxElementNode
                  kind = Variable(SymbolType.make (tokenFromSyntaxTypeNode syntaxElementNode) (Some []))
                  entries = []
                  tree = syntaxElementNode
                  globalTree = None }

            Some symbolEntry,
            { root =
                  { syntaxElement = syntaxElement
                    symbolEntry = Some symbolEntry }
              children =
                  List.flatten
                      [ syntaxElementNode.children
                        |> List.map makeSymbolEntry
                        |> List.map snd ] }
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
                { name =
                      { tokenType = TokenType.Id "ClassDeclList"
                        location =
                            { line = 0
                              column = 0 } }
                  kind = Nil
                  entries = symbolClassDeclEntries
                  tree = syntaxClassDeclListNode
                  globalTree = None }

            let symbolClassDeclListTree =
                { root =
                      { syntaxElement = tree.children.[0].root
                        symbolEntry = Some symbolClassDeclListEntry }
                  children = List.map snd symbolClassDeclEntryAndTreePairs }

            let symbolFuncDefListEntry =
                { name =
                      { tokenType = TokenType.Id "FuncDefList"
                        location =
                            { line = 0
                              column = 0 } }
                  kind = Nil
                  entries = symbolFuncDefEntries
                  tree = syntaxFuncDefListNode
                  globalTree = None }

            let symbolFuncDefListTree =
                { root =
                      { syntaxElement = tree.children.[1].root
                        symbolEntry = Some symbolFuncDefListEntry }
                  children = List.map snd symbolFuncDefEntryAndTreePairs }

            let symbolMainFuncBodyEntry =
                { name =
                      { tokenType = TokenType.Id "MainFuncBody"
                        location =
                            { line = 0
                              column = 0 } }
                  kind = Nil
                  entries = symbolMainFuncBodyEntries
                  tree = syntaxMainFuncBodyNode
                  globalTree = None }

            let symbolMainFuncBodyTree =
                { root =
                      { syntaxElement = tree.children.[2].root
                        symbolEntry = Some symbolMainFuncBodyEntry }
                  children = List.map snd symbolMainFuncBodyEntryAndTreePairs }

            let symbolProgEntry =
                { name =
                      { tokenType = TokenType.Id "Prog"
                        location =
                            { line = 0
                              column = 0 } }
                  kind = Nil
                  entries = symbolClassDeclEntries @ symbolFuncDefEntries @ [ symbolMainFuncBodyEntry ]
                  tree = tree
                  globalTree = None }

            let symbolTree =
                { root =
                      { syntaxElement =
                            { syntaxKind = Prog
                              token = None }
                        symbolEntry = Some symbolProgEntry }
                  children = [ symbolClassDeclListTree; symbolFuncDefListTree; symbolMainFuncBodyTree ] }

            let symbolTable = { symbolProgEntry with globalTree = Some symbolTree }
            let symbolEntry = { symbolTree.root with symbolEntry = Some symbolTable }
            symbolTable, Tree.create symbolEntry symbolTree.children
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

type SymbolTable with
    member x.findLocalSymbolTable symbolTree =
        SymbolTable.findSymbolTable symbolTree x

    member x.findClassSymbolTable symbolTree =
        SymbolTable.tryFindClassTable symbolTree x
