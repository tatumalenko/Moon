namespace Moon

[<StructuredFormatDisplay("{show}")>]
type Node<'E> =
    { index: int
      mutable parent: int option
      mutable branches: int list
      element: 'E }
    member x.show =
        let asStringOrNull arg =
            if Option.isSome arg then show (Option.get arg) else ""

        let childrenAsString = "[" + (String.concat ", " (x.branches |> List.map (fun e -> show e))) + "]"
        "(index: " + show x.index + ", parent: " + (asStringOrNull x.parent) + ", children: " + childrenAsString + ", element: " + show x.element
        + ")"

and SyntaxKind =
    | Data
    | Epsilon
    | Id
    | Idi
    | Num
    | RelOp
    | Type
    | ClassDeclList
    | FuncDefList
    | Prog
    | MainFuncBody
    | MemberDeclList
    | ClassDecl
    | FuncDecl
    | FuncDef
    | FuncBody
    | StatBlock
    | VarDecl
    | DimList
    | AssignStat
    | IfStat
    | WhileStat
    | ReadStat
    | WriteStat
    | ReturnStat
    | IndexList
    | RelExpr
    | AddOp
    | MultOp
    | Not
    | Sign
    | VarElementList
    | DataMember
    | FunctionCall
    | InheritList
    | FParam
    | FParamList
    | AParamList
    | EndProgram

    static member fromString (text: string) =
        match text with
        | "#GetData" -> Ok Data
        | "#MakeNodeEpsilon" -> Ok Epsilon
        | "#MakeNodeId" -> Ok Id
        | "#MakeNodeIdi" -> Ok Idi
        | "#MakeNodeNum" -> Ok Num
        | "#MakeNodeRelOp" -> Ok RelOp
        | "#MakeNodeType" -> Ok Type
        | "#MakeNodeClassDeclList" -> Ok ClassDeclList
        | "#MakeNodeFuncDefList" -> Ok FuncDefList
        | "#MakeNodeProg" -> Ok Prog
        | "#MakeNodeMemberDeclList" -> Ok MemberDeclList
        | "#MakeNodeClassDecl" -> Ok ClassDecl
        | "#MakeNodeFuncDecl" -> Ok FuncDecl
        | "#MakeNodeFuncDef" -> Ok FuncDef
        | "#MakeNodeMainFuncBody" -> Ok MainFuncBody
        | "#MakeNodeFuncBody" -> Ok FuncBody
        | "#MakeNodeStatBlock" -> Ok StatBlock
        | "#MakeNodeVarDecl" -> Ok VarDecl
        | "#MakeNodeDimList" -> Ok DimList
        | "#MakeNodeAssignStat" -> Ok AssignStat
        | "#MakeNodeIfStat" -> Ok IfStat
        | "#MakeNodeWhileStat" -> Ok WhileStat
        | "#MakeNodeReadStat" -> Ok ReadStat
        | "#MakeNodeWriteStat" -> Ok WriteStat
        | "#MakeNodeReturnStat" -> Ok ReturnStat
        | "#MakeNodeIndexList" -> Ok IndexList
        | "#MakeNodeRelExpr" -> Ok RelExpr
        | "#MakeNodeAddOp" -> Ok AddOp
        | "#MakeNodeMultOp" -> Ok MultOp
        | "#MakeNodeNot" -> Ok Not
        | "#MakeNodeSign" -> Ok Sign
        | "#MakeNodeVarElementList" -> Ok VarElementList
        | "#MakeNodeDataMember" -> Ok DataMember
        | "#MakeNodeFunctionCall" -> Ok FunctionCall
        | "#MakeNodeInheritList" -> Ok InheritList
        | "#MakeNodeFParam" -> Ok FParam
        | "#MakeNodeFParamList" -> Ok FParamList
        | "#MakeNodeAParamList" -> Ok AParamList
        | "#EndProgram" -> Ok EndProgram
        | _ -> Error(show text)

    member m.needData =
        match m with
        | Id
        | Idi
        | Num
        | RelOp
        | Type
        | AddOp
        | MultOp
        | Not
        | Sign -> true
        | _ -> false

    member m.children =
        let expr = [ RelExpr; AddOp; MultOp; VarElementList; Num; Not; Sign ]
        let arithExpr = [ AddOp; MultOp; VarElementList; Num; Not; Sign ]
        let term = [ AddOp; MultOp; VarElementList; Num; Not; Sign ]
        let factor = [ AddOp; MultOp; VarElementList; Num; Not; Sign ]

        match m with
        | Data
        | Epsilon
        | Id
        | Idi
        | Num
        | RelOp
        | Type -> Leaf
        | ClassDeclList -> Single(Many ClassDecl)
        | FuncDefList -> Single(Many FuncDef)
        | Prog ->
            List
                [ One ClassDeclList
                  One FuncDefList
                  One MainFuncBody ]
        | MemberDeclList -> Single(ManyOf [ VarDecl; FuncDecl ])
        | ClassDecl ->
            List
                [ One Id
                  One InheritList
                  One MemberDeclList ]
        | FuncDecl ->
            List
                [ One Id
                  One FParamList
                  One Type ]
        | FuncDef ->
            List
                [ OneOf [ Type; Epsilon ]
                  One Id
                  One FParamList
                  One Type
                  One FuncBody ]
        | MainFuncBody -> Single(ManyOf [ VarDecl; FunctionCall; AssignStat; IfStat; WhileStat; ReadStat; WriteStat; ReturnStat ])
        | FuncBody -> Single(ManyOf [ VarDecl; FunctionCall; AssignStat; IfStat; WhileStat; ReadStat; WriteStat; ReturnStat ])
        | StatBlock -> Single(ManyOf [ FunctionCall; AssignStat; IfStat; WhileStat; ReadStat; WriteStat; ReturnStat ])
        | VarDecl ->
            List
                [ One Type
                  One Id
                  ManyOf [ DimList ] ]
        | DimList -> Single(Many Num)
        | AssignStat ->
            List
                [ One VarElementList
                  OneOf expr ]
        | IfStat ->
            List
                [ One RelExpr
                  One StatBlock
                  One StatBlock ]
        | WhileStat ->
            List
                [ One RelExpr
                  One StatBlock ]
        | ReadStat -> Single(One VarElementList)
        | WriteStat -> Single(OneOf expr)
        | ReturnStat -> Single(OneOf expr)
        | IndexList -> Single(ManyOf arithExpr) // TODO: Changed from expr
        | RelExpr ->
            List
                [ OneOf arithExpr // TODO: Changed from expr
                  One RelOp
                  OneOf arithExpr ] // TODO: Changed from expr
        | AddOp ->
            List
                [ OneOf arithExpr
                  OneOf term ]
        | MultOp ->
            List
                [ OneOf term
                  OneOf factor ]
        | Not -> Single(OneOf factor)
        | Sign -> Single(OneOf factor)
        | VarElementList -> Single(ManyOf [ DataMember; FunctionCall ])
        | DataMember ->
            List
                [ One Id
                  One IndexList ]
        | FunctionCall ->
            List
                [ One Id
                  One AParamList ]
        | InheritList -> Single(Many Idi)
        | FParam ->
            List
                [ One Type
                  One Id
                  ManyOf [ DimList ] ]
        | FParamList -> Single(Many FParam)
        | AParamList -> Single(ManyOf expr)
        | EndProgram -> Leaf

and NodeChildren =
    | Single of NodeChildrenGroup
    | List of NodeChildrenGroup list
    | Leaf

and NodeChildrenGroup =
    | One of SyntaxKind
    | OneOf of SyntaxKind list
    | Many of SyntaxKind
    | ManyOf of SyntaxKind list

and [<StructuredFormatDisplay("{show}")>] SyntaxElement =
    { syntaxKind: SyntaxKind
      token: Token option }

    static member inline make syntaxKind token =
        { syntaxKind = syntaxKind
          token = token }

    member inline x.show = x.token.map (fun it -> show it.case + " (" + it.lexeme + ")") @? show x.syntaxKind

and IndexedAst =
    { mutable index: int option
      mutable nodes: Node<SyntaxElement> list
      mutable errors: ASTError list }

    static member inline make: IndexedAst =
        { index = None
          nodes = List.empty
          errors = List.empty }

    member m.nodeAt (nodeIndex: int) = m.nodes.[nodeIndex]

    member m.nodeTypeAt (nodeIndex: int) = (m.nodeElementAt nodeIndex).syntaxKind

    member m.nodeElementAt (nodeId: int) = m.nodes.[nodeId].element

    member m.childrenAt (nodeId: int) = m.nodes.[nodeId].branches

    member m.addLeftChild (parentId: int) (childId: int) =
        let mutable parentNode = m.nodes.[parentId]
        List.insert (childId) 0 (&parentNode.branches)
        let childNode = m.nodes.[childId]
        childNode.parent <- Some parentId

    member m.addOne (newNodeId: int) (nodeType: SyntaxKind) (topNodeId: int) =
        if m.nodeTypeAt topNodeId = nodeType then
            m.addLeftChild newNodeId topNodeId
            true
        else
            false

    member m.addOneOf (newNodeId: int) (nodeList: SyntaxKind list) (topNodeId: int) =
        if List.contains (m.nodeTypeAt topNodeId) nodeList then
            m.addLeftChild newNodeId topNodeId
            true
        else
            false

    member m.tryAddNodeChildren (semanticStack: int list byref, newNodeType: SyntaxKind, newNodeId: int, nodeChildrenGroup: NodeChildrenGroup) =
        let mutable result: Result<unit, ASTError> = Ok()
        let mutable tos = List.pop &semanticStack

        let expectedNodeType =
            match nodeChildrenGroup with
            | One nodeType
            | Many nodeType -> [ nodeType ]
            | OneOf nodeList
            | ManyOf nodeList -> nodeList

        match nodeChildrenGroup with
        | One nodeType ->
            match tos with
            | Some topNodeId ->
                if not (m.addOne newNodeId nodeType topNodeId) then
                    result <- Error(WrongNodeOnStackOne(newNodeType, nodeType, m.nodeTypeAt topNodeId))
            | None ->
                result <- Error(NoNodeOnStackOne(newNodeType, nodeType))
        | OneOf nodeList ->
            match tos with
            | Some topNodeId ->
                if not (m.addOneOf newNodeId nodeList topNodeId) then
                    result <- Error(WrongNodeOnStackList(newNodeType, nodeList, m.nodeTypeAt topNodeId))
            | None ->
                result <- Error(NoNodeOnStackList(newNodeType, nodeList))
        | Many nodeType ->
            let mutable keepGoing = true
            while keepGoing do
                match tos with
                | Some topNodeId ->
                    if not (m.addOne newNodeId nodeType topNodeId) then
                        semanticStack <- semanticStack @ [ topNodeId ]
                        keepGoing <- false
                    else
                        tos <- List.pop &semanticStack
                | None -> keepGoing <- false
        | ManyOf nodeList ->
            let mutable keepGoing = true
            while keepGoing do
                match tos with
                | Some topNodeId ->
                    if not (m.addOneOf newNodeId nodeList topNodeId) then
                        semanticStack <- semanticStack @ [ topNodeId ]
                        keepGoing <- false
                    else
                        tos <- List.pop &semanticStack
                | None -> keepGoing <- false

        result

    member m.makeNode (semanticStack: int list byref, tokenStack: Token list byref, newNodeType: SyntaxKind) =
        let mutable result: Result<unit, ASTError> = Ok()
        let mutable tokenMaybe = None

        if newNodeType.needData then
            match List.pop &tokenStack with
            | Some token -> tokenMaybe <- Some token
            | None -> result <- Error(EmptyTokenStack newNodeType)

        match result with
        | Ok _ ->
            let newNodeId =
                m.newNode
                    ({ syntaxKind = newNodeType
                       token = tokenMaybe })

            let nodeChildren = newNodeType.children
            match nodeChildren with
            | Single nodeChildrenGroup ->
                match m.tryAddNodeChildren (&semanticStack, newNodeType, newNodeId, nodeChildrenGroup) with
                | Ok _ -> ()
                | Error failure ->
                    result <- Error failure
            | List nodeChildrenGroupList ->
                let mutable keepGoing = true
                let nodeChildrenGroupListIter = Iter.make (List.rev nodeChildrenGroupList)
                while keepGoing do
                    let nodeChildrenGroup = nodeChildrenGroupListIter.next()
                    match nodeChildrenGroup with
                    | Some nodeChildrenGroup ->
                        match m.tryAddNodeChildren (&semanticStack, newNodeType, newNodeId, nodeChildrenGroup) with
                        | Ok _ -> ()
                        | Error failure ->
                            result <- Error failure
                            keepGoing <- false
                    | None -> keepGoing <- false
            | Leaf -> ()

            match result with
            | Ok _ ->
                semanticStack <- semanticStack @ [ newNodeId ]
            | Error _ -> ()
        | Error _ -> () // Let the previous error assignment trickle down to return

        result

    member m.newNode (nodeElement: SyntaxElement) =
        let index = List.length m.nodes

        let node =
            { index = index
              parent = None
              branches = List.empty
              element = nodeElement }
        m.nodes <- m.nodes @ [ node ]
        index

and [<StructuredFormatDisplay("{show}")>] ASTError =
    | EmptyTokenStack of SyntaxKind
    | NoNodeOnStackOne of SyntaxKind * SyntaxKind
    | NoNodeOnStackList of SyntaxKind * SyntaxKind list
    | WrongNodeOnStackOne of SyntaxKind * SyntaxKind * SyntaxKind
    | WrongNodeOnStackList of SyntaxKind * SyntaxKind list * SyntaxKind
    member x.show =
        match x with
        | EmptyTokenStack nodeType ->
            "EmptyTokenStack (No token found on token stack to make node: " + show nodeType + ")"
        | NoNodeOnStackOne(nodeType, childNodeType) ->
            "NoNodeOnStackOne (Trying to make node " + show nodeType + " while expecting " + show childNodeType + ", but the stack is empty)"
        | NoNodeOnStackList(nodeType, nodeList) ->
            "NoNodeOnStackList (Trying to make node " + show nodeType + ", while expecting any of ["
            + (List.map (fun e -> show e) nodeList |> String.concat ", ") + "], but the stack is empty)"
        | WrongNodeOnStackOne(nodeType, childNodeType, stackTopNodeType) ->
            "WrongNodeOnStackOne (Trying to make node " + show nodeType + " while expecting " + show childNodeType + ", but found node "
            + show stackTopNodeType + ")"
        | WrongNodeOnStackList(nodeType, nodeList, stackTopNodeType) ->
            "WrongNodeOnStackList (Trying to make node " + show nodeType + ", while expecting any of ["
            + (List.map (fun e -> show e) nodeList |> String.concat ", ") + "], but found node " + show stackTopNodeType + ")"

[<RequireQualifiedAccess>]
module Ast =
    open FSharpPlus

    let asGraphViz (ast: IndexedAst): string =
        let mutable result: Result<string, string> = Ok ""

        let mutable str = ""
        str <- str + "digraph AST {\n"
        str <- str + "    node [shape=record];\n"
        str <- str + "    node [frontname=Sans];\n"
        str <- str + "    charset=\"UTF-8\"\n"
        str <- str + "    splines=true\n"
        str <- str + "    splines=spline\n"
        str <- str + "    rankdir=LR\n\n"

        let rec asGraphVizNode (nodeIndex: int) =
            str <- str + "    node [label=\"" + show (ast.nodeElementAt nodeIndex) + "\"] " + show nodeIndex + "\n"
            for childIndex in ast.childrenAt nodeIndex do
                asGraphVizNode childIndex

        let rec asGraphVizEdge (nodeIndex: int) =
            for childIndex in ast.childrenAt nodeIndex do
                str <- str + "    " + show nodeIndex + " -> " + show childIndex + "\n"
                asGraphVizEdge childIndex

        match ast.index with
        | Some rootIndex ->
            asGraphVizNode rootIndex
            asGraphVizEdge rootIndex
            str <- str + "}\n"
            str <- str.Replace("||", "\\|\\|")
            str <- str.Replace("<", "\\<")
            str <- str.Replace(" >", " \\>")
            str <- str.Replace("(>)", "\\(\\>\\)")
            str <- str.Replace("<>", "<\\>")
            str <- str.Replace(">=", "\\>\\=")
            str <- str.Replace("{ ", "\\{ ")
            str <- str.Replace(" }", " \\}")
        | None -> result <- Error "AST has no root!"

        str

    let makeTree (ast: IndexedAst) =
        let f x = (x, List.map ast.nodeAt x.branches)
        let g x = x.element
        Tree.unfold f (ast.nodeAt (Option.get ast.index)) |> Tree.map g

    let makeGraphViz (ast: Tree<'Element>) =
        let foldWithIndex id syntaxElement = id + 1, (id, syntaxElement)
        let treeWithIndex = Tree.mapFold foldWithIndex 0 ast |> snd

        let foldNode s root = "    node [label=\"" + show (snd root) + "\"] " + show (fst root) + "\n" + s
        let foldEdge s parentRoot childRoot = s + "    " + show (fst parentRoot) + " -> " + show (fst childRoot) + "\n"

        let gvNodes =
            Tree.fold foldNode "" treeWithIndex
            |> String.replaceWith [ "||"; "<"; " >"; "(>)"; "<>"; ">="; "{ "; " }" ]
                   [ "\\|\\|"; "\\<"; " \\>"; "\\(\\>\\)"; "<\\>"; "\\>\\="; "\\{ "; " \\}" ]
        let gvEdges = Tree.foldWithParent foldEdge "" treeWithIndex

        "}"
        |> (+) gvEdges
        |> (+) gvNodes
        |> (+) "    rankdir=LR\n\n"
        |> (+) "    graph [ordering=\"out\"];\n"
        |> (+) "    node [shape=record];\n"
        |> (+) "digraph AST {\n"
