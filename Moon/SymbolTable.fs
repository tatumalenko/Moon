namespace Moon

open System.Collections.Generic
open System.Reflection.Metadata

type Dimensionality =
    | Scalar
    | Array of int

[<StructuredFormatDisplay("{show}")>]
type SymbolType =
    | Integer of int list option
    | Float of int list option
    | Class of string * int list option
    | Void
    | Nil

    member x.withoutDimensionality =
        match x with
        | Integer _ -> Integer None
        | Float _ -> Float None
        | Class (className, _) -> Class(className, None)
        | Void
        | Nil -> failwith "SymbolType.dimensions: Tried to get dimensions of non-Integer, Float, or Class SymbolType"

    member x.dimensions =
        match x with
        | Integer dimensions
        | Float dimensions
        | Class (_, dimensions) -> dimensions
        | Void
        | Nil -> failwith "SymbolType.dimensions: Tried to get dimensions of non-Integer, Float, or Class SymbolType"

    member x.className =
        match x with
        | Class (className, _) -> className
        | Integer _
        | Float _
        | Void
        | Nil -> "<unresolved>"

    static member make(typeToken: Token, dimensions: int list option) =
        match typeToken.lexeme with
        | "integer" -> Integer dimensions
        | "float" -> Float dimensions
        | "void" -> Void
        | Int _ -> Integer None
        | Double _ -> Float None
        | className -> Class(className, dimensions)

    static member make(symbolType: SymbolType, dimensions: int list option) =
        match symbolType with
        | Integer _ -> Integer dimensions
        | Float _ -> Float dimensions
        | Class (className, _) -> Class(className, dimensions)
        | Void -> Void
        | Nil -> Nil

    static member make(symbolType: SymbolType, dimensionality: int) =
        match symbolType with
        | Integer _ ->
            Integer(if dimensionality > 0 then Some(List.replicate dimensionality -1) else None)
        | Float _ ->
            Float(if dimensionality > 0 then Some(List.replicate dimensionality -1) else None)
        | Class (className, _) ->
            Class
                (className,
                 (if dimensionality > 0 then Some(List.replicate dimensionality -1) else None))
        | Void -> Void
        | Nil -> Nil

    static member create (typeName: string) (dimensions: int list) =
        match typeName with
        | "integer" -> Integer(Some dimensions)
        | "float" -> Float(Some dimensions)
        | "void" -> Void
        | Int _ -> Integer None
        | Double _ -> Float None
        | className -> Class(className, Some dimensions)

    member x.fakeToken =
        let fakeLexeme =
            match x with
            | Integer _ -> "integer"
            | Float _ -> "float"
            | Class (className, _) -> className
            | Void -> "void"
            | Nil -> ""
        { tokenType = TokenType.Id fakeLexeme
          location =
              { line = 0
                column = 0 } }

    member x.dimensionality =
        match x.dimensions with
        | None -> Dimensionality.Scalar
        | Some xs -> Dimensionality.Array(List.length xs)

    member inline x.show =
        match x with
        | Integer dimensions -> "integer" + (dimensions.map (fun dims -> List.fold (fun s e -> s + "[" + show e + "]") "" dims) @? "")
        | Float dimensions -> "float" + (dimensions.map (fun dims -> List.fold (fun s e -> s + "[" + show e + "]") "" dims) @? "")
        | Class (className, dimensions) -> className + (dimensions.map (fun dims -> List.fold (fun s e -> s + "[" + show e + "]") "" dims) @? "")
        | Void -> "void"
        | Nil -> ""

type SymbolKindType =
    | Variable
    | Parameter
    | FreeFunction
    | MemberFunction
    | Class
    | ProgKind
    | Nil

[<StructuredFormatDisplay("{show}")>]
type SymbolKind =
    | Variable of SymbolType
    | Parameter of SymbolType
    | FreeFunction of SymbolType * SymbolType list // returnType, paramType[]
    | MemberFunction of SymbolType * SymbolType list * SymbolType // returnType, paramType[], classType
    | Class of SymbolType list // superType[]
    | ProgKind
    | Nil

    member x.symbolKindType =
        match x with
        | Variable _ -> SymbolKindType.Variable
        | Parameter _ -> SymbolKindType.Parameter
        | FreeFunction _ -> SymbolKindType.FreeFunction
        | MemberFunction _ -> SymbolKindType.MemberFunction
        | Class _ -> SymbolKindType.Class
        | ProgKind -> SymbolKindType.ProgKind
        | Nil -> SymbolKindType.Nil

    member x.withNoDimensionality =
        match x with
        | Variable symbolType -> Variable symbolType.withoutDimensionality
        | Parameter symbolType -> Parameter symbolType.withoutDimensionality
        | FreeFunction (symbolType, _) -> FreeFunction(symbolType.withoutDimensionality, [])
        | MemberFunction (symbolType, _, superTypes) -> MemberFunction(symbolType.withoutDimensionality, [], superTypes)
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
        | FreeFunction (symbolType, _) -> Some symbolType
        | MemberFunction (symbolType, _, _) -> Some symbolType
        | Variable _
        | Parameter _
        | Class _
        | ProgKind
        | Nil -> failwith "SymbolTable.returnType: Tried to access returnType of non-Function SymbolKind"

    member x.paramTypes =
        match x with
        | FreeFunction (_, symbolTypes) -> symbolTypes
        | MemberFunction (_, symbolTypes, _) -> symbolTypes
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
        | FreeFunction (returnType, paramTypes) -> "(" + String.concat ", " (List.map show paramTypes) + "): " + show returnType
        | MemberFunction (returnType, paramTypes, _) -> "(" + String.concat ", " (List.map show paramTypes) + "): " + show returnType
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

    member x.symbolTree = x.globalTree @! "SymbolTable.symbolTree: Tried to get `globalTree` but was None"

    member x.withNoDimensionality = { x with kind = x.kind.withNoDimensionality }

    member x.symbolType =
        match x.kind with
        | Variable symbolType
        | Parameter symbolType -> Some symbolType
        | FreeFunction (_, _) -> x.returnType
        | MemberFunction (_, _, _) -> x.returnType
        | Class _ -> None
        | ProgKind -> None
        | Nil -> None

    member x.returnType =
        match x.kind with
        | FreeFunction (symbolType, _) -> Some symbolType
        | MemberFunction (symbolType, _, _) -> Some symbolType
        | Variable _
        | Parameter _
        | Class _
        | ProgKind
        | Nil -> failwith "SymbolTable.returnType: Tried to access returnType of non-Function SymbolKind"

    member x.paramTypes =
        match x.kind with
        | FreeFunction (_, symbolTypes) -> symbolTypes
        | MemberFunction (_, symbolTypes, _) -> symbolTypes
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

    member x.duplicateByErrorComparer =
        match x.kind with
        | Variable _ -> x.name.lexeme
        | Parameter _ -> x.name.lexeme
        | FreeFunction _ -> "Function " + x.name.lexeme + "(" + String.concat ", " (List.map show x.paramTypes) + ")"
        | MemberFunction (_, _, classType) ->
            "Function " + show classType + "." + x.name.lexeme + "(" + String.concat ", " (List.map show x.paramTypes) + ")"
        | Class _ -> x.name.lexeme
        | ProgKind
        | Nil -> ""

    member x.duplicateByWarningComparer =
        match x.kind with
        | Variable _ -> x.name.lexeme
        | Parameter _ -> x.name.lexeme
        | FreeFunction _ -> "Function " + x.name.lexeme + "(" + String.concat ", " (List.map show x.paramTypes) + ")"
        | MemberFunction (_, _, classType) -> "Function " + show classType + "." + x.name.lexeme + "(" + ")"
        | Class _ -> x.name.lexeme
        | ProgKind
        | Nil -> failwith "Semanter.SymbolCheckVisitor.checkMultiplyDeclared: `ProgKind` or `Nil` type found"

    member inline x.show =
        match x.kind with
        | Variable _ -> "Variable " + x.name.lexeme + ": " + show x.kind
        | Parameter _ -> "Parameter " + x.name.lexeme + ": " + show x.kind
        | FreeFunction (_, _) -> "Function " + x.name.lexeme + show x.kind
        | MemberFunction (_, _, classType) -> "Function " + show classType + "." + x.name.lexeme + show x.kind
        | Class _ -> "Class " + x.name.lexeme + show x.kind
        | ProgKind -> "Prog"
        | Nil -> x.name.lexeme

and [<StructuredFormatDisplay("{show}")>] SymbolElement =
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
    let create name kind =
        let token =
            { tokenType = TokenType.Id name
              location =
                  { line = 0
                    column = 0 } }
        { name = token
          kind = kind
          entries = []
          tree =
              Tree.create
                  { syntaxKind = SyntaxKind.Id
                    token = None } []
          globalTree = None }

    let empty =
        create "" SymbolKind.Nil

    let rec findSymbolTables (localScope: SymbolTable option) treeToFind currentTree =
        let localScope =
            match currentTree.root.syntaxElement.syntaxKind with
            | Prog -> (currentTree.root.symbolEntry)
            | FuncDef -> (currentTree.root.symbolEntry)
            | ClassDecl -> currentTree.root.symbolEntry
            | MainFuncBody -> (currentTree.root.symbolEntry)
            | _ -> localScope
        if treeToFind = currentTree then
            localScope
        else
            let localScopes = List.map (findSymbolTables localScope treeToFind) currentTree.children
            if List.length (List.choose id localScopes) > 1
            then printfn "hey"
            else ()
            List.choose id localScopes |> List.tryHead

    let rec findSymbolTable (treeToFind: Tree<SymbolElement>) (symbolTable: SymbolTable) =
        let localScope = findSymbolTables None treeToFind symbolTable.symbolTree
        localScope

    let rec map (mapper: _ -> _) (symbolTable: SymbolTable): SymbolTable =
        match symbolTable.entries with
        | [] -> mapper symbolTable
        | xs -> { mapper symbolTable with entries = List.map mapper xs }

    let tryFind (predicate: _ -> bool) (symbolTable: SymbolTable) =
        let rec f (x: SymbolTable option): SymbolTable option =
            match x.map (fun it -> it.entries, predicate it) with
            | Some (_, true) -> x
            | Some (xs, false) -> List.tryHead (List.choose id (List.map (f << Some) xs))
            | None -> None
        f (Some symbolTable)

    let filter (predicate: _ -> bool) (symbolTable: SymbolTable): SymbolTable list =
        let rec f (x: SymbolTable option): SymbolTable list =
            match x.map (fun it -> it.entries, predicate it) with
            | Some (_, true) -> x.map (fun it -> [ it ]) @? []
            | Some (xs, false) -> List.flatMap (f << Some) xs
            | None -> []
        f (Some symbolTable)

    let tryFindClassTable (tree: Tree<SymbolElement>) (symbolTable: SymbolTable): SymbolTable option =
        let symbolTableSuperTypes =
            match symbolTable.kind with
            | Class superTypes -> List.map (fun (it: SymbolType) -> it.className) superTypes
            | _ -> []

        let finderPredicate (className: string) (st: SymbolTable) =
            className = st.name.lexeme || List.contains st.name.lexeme symbolTableSuperTypes

        let localScope = findSymbolTable tree symbolTable
        match localScope.map (fun it -> it.kind) with
        | Some (SymbolKind.MemberFunction (returnType, paramTypes, classType)) ->
            tryFind (finderPredicate classType.className) symbolTable
        | _ -> None

    let tryFindTableWithName (idSyntaxToken: Token) (symbolTable: SymbolTable) =
        tryFind (fun st -> st.name.lexeme = idSyntaxToken.lexeme) symbolTable

    let tryFindTableWithName2 (name: string) (symbolTable: SymbolTable) =
        tryFind (fun st -> st.name.lexeme = name) symbolTable

    let tryFindTableWithNameAndKind (name: string) (kindType: SymbolKindType) (symbolTable: SymbolTable) =
        tryFind (fun st -> st.name.lexeme = name && st.kind.symbolKindType = kindType) symbolTable

    let tryFindClassTables (tree: Tree<SymbolElement>) (symbolTable: SymbolTable): SymbolTable list =
        let classSymbolTableMaybe = tryFindClassTable tree symbolTable

        match classSymbolTableMaybe.map (fun it -> it, it.kind) with
        | Some (classSymbolTable, Class superTypes) ->
            let superClassTables = List.map (fun (it: SymbolType) -> tryFindTableWithName2 it.className symbolTable) superTypes |> List.choose id
            classSymbolTable :: superClassTables
        | _ -> []

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
            let dimList = List.map (int << nameFromSyntaxIdNode) syntaxNumNodes
            Some dimList
        | None -> None

    let intListFromSyntaxDimListNodes (nodes: Tree<SyntaxElement> list) =
        match nodes with
        | [] ->
            None
        | xs ->
            let syntaxNumNodes = List.flatMap (fun e -> e.children) xs
            match List.length syntaxNumNodes with
            | 0 -> Some(List.replicate (List.length xs) -1)
            | _ ->
                let dimList = List.map (int << nameFromSyntaxIdNode) syntaxNumNodes
                Some dimList

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
            let syntaxDimListNodes = List.range 2 syntaxElementNode.children

            let symbolEntry =
                { name = tokenFromSyntaxIdNode syntaxIdNode
                  kind = Variable(SymbolType.make (tokenFromSyntaxTypeNode syntaxTypeNode, intListFromSyntaxDimListNodes syntaxDimListNodes))
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
            let syntaxDimListNodes = List.range 2 syntaxElementNode.children

            let symbolEntry =
                { name = tokenFromSyntaxIdNode syntaxIdNode
                  kind = Parameter(SymbolType.make (tokenFromSyntaxTypeNode syntaxTypeNode, intListFromSyntaxDimListNodes syntaxDimListNodes))
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
                  kind = FreeFunction(SymbolType.make (tokenFromSyntaxTypeNode syntaxReturnTypeNode, None), List.choose id paramFParamTypes)
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
                | FreeFunction (returnType, paramTypes) ->
                    { name = entry.name
                      kind = MemberFunction(returnType, paramTypes, SymbolType.Class(nameFromSyntaxIdNode syntaxIdNode, None))
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
                           |> List.map (fun superTypeName -> SymbolType.Class(superTypeName, None)))
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
                              (SymbolType.make (tokenFromSyntaxTypeNode syntaxReturnTypeNode, None),
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
                              (SymbolType.make (tokenFromSyntaxTypeNode syntaxReturnTypeNode, None),
                               symbolTypesFromSyntaxFParamListNode syntaxFParamListNode |> List.map (Option.get << fst),
                               SymbolType.make (tokenFromSyntaxTypeNode syntaxTypeOrEpsilonNode, None))
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
                  kind = Variable(SymbolType.make (tokenFromSyntaxTypeNode syntaxElementNode, None))
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
        | _ ->
            empty,
            Tree.create
                { syntaxElement =
                      { syntaxKind = Prog
                        token = None }
                  symbolEntry = None } []

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
    member x.tryFindLocalTable symbolTree = SymbolTable.findSymbolTable symbolTree x

    member x.tryFindClassTable symbolTree = SymbolTable.tryFindClassTable symbolTree x

    member x.tryFindTableWithName token = SymbolTable.tryFindTableWithName token x

    member x.findClassTables symbolTree = SymbolTable.tryFindClassTables symbolTree x

    ///
    /// Returns localTable, classTable, and classSuperTables
    member x.tryFindTables tree =
        let localTableMaybe = x.tryFindLocalTable tree
        let classTableMaybe = x.tryFindClassTable tree
        let classSuperTables = x.findClassTables tree
        localTableMaybe, classTableMaybe, classSuperTables

    member x.tryFindTableWithNameAndKind name kind = SymbolTable.tryFindTableWithNameAndKind name kind x

    member x.tryFindFunctionTable tree =
        let localTable, classTable, superTables = x.tryFindTables tree

        let freeFunctionTable =
            match List.tryItem 0 tree.children with
            | Some idNode ->
                x.tryFindTableWithNameAndKind idNode.root.syntaxToken.lexeme SymbolKindType.FreeFunction
            | _ -> None

        match classTable, superTables, freeFunctionTable with
        | Some classTable, _, _ -> Some classTable
        | _, superTables, _ when not (List.isEmpty superTables) -> Some(List.last superTables)
        | _, _, Some freeFunctionTable -> Some freeFunctionTable
        | _ -> None

    member x.numberOfParameters =
        let countParameters state (item: SymbolTable) =
            let adder =
                match item.kind with
                | Parameter _ -> 1
                | _ -> 0
            state + adder
        List.fold countParameters 0 x.entries

type SymbolTableComparer(comparer: SymbolTable -> SymbolTable -> bool, hasher: SymbolTable -> int) =
    member this.comparer = comparer

    member this.hasher = hasher

    interface IEqualityComparer<SymbolTable> with
        member this.Equals(x, y) = this.comparer x y

        member this.GetHashCode(x) = this.hasher x
