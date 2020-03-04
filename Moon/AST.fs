module Moon.AST

open Moon.Extensions

type Node<'E> =
    {
        index: int
        mutable parent: int option
        mutable children: int list
        element: 'E
    }
    override m.ToString() =
        let asStringOrNull arg = if Option.isSome arg then arg.ToString() else "null"
        let childrenAsString = "[" + (String.concat ", " (m.children |> List.map (fun e -> e.ToString()))) + "]"
        "(index: " + m.index.ToString() + ", parent: " + (asStringOrNull m.parent) + ", children: " + childrenAsString + ", element: " + m.element.ToString() + ")"
    
and NodeType =
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
    | MemberList
    | ClassDecl
    | FuncDecl
    | FuncDef
    | FuncBody
    | StatBlock
    | VarDecl
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
        | "#MakeNodeId" -> Ok Id
        | "#MakeNodeIdi" -> Ok Idi
        | "#MakeNodeNum" -> Ok Num
        | "#MakeNodeRelOp" -> Ok RelOp
        | "#MakeNodeType" -> Ok Type
        | "#MakeNodeClassDeclList" -> Ok ClassDeclList
        | "#MakeNodeFuncDefList" -> Ok FuncDefList
        | "#MakeNodeProg" -> Ok Prog
        | "#MakeNodeMemberList" -> Ok MemberList
        | "#MakeNodeClassDecl" -> Ok ClassDecl
        | "#MakeNodeFuncDecl" -> Ok FuncDecl
        | "#MakeNodeFuncDef" -> Ok FuncDef
        | "#MakeNodeMainFuncBody" -> Ok MainFuncBody
        | "#MakeNodeFuncBody" -> Ok FuncBody
        | "#MakeNodeStatBlock" -> Ok StatBlock
        | "#MakeNodeVarDecl" -> Ok VarDecl
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
        | "#MakeNodeEndProgram" -> Ok EndProgram
        | _ -> Error (text.ToString())
    
    member m.needData =
        match m with
        | Id | Idi | Num | RelOp | Type | AddOp | MultOp | Not | Sign -> true
        | _ -> false
        
    member m.children =
        let expr = [ RelExpr; AddOp; MultOp; VarElementList; Num; Not; Sign ]
        let arithExpr = [ AddOp; MultOp; VarElementList; Num; Not; Sign ]
        let term = [ AddOp; MultOp; VarElementList; Num; Not; Sign ]
        let factor = [ AddOp; MultOp; VarElementList; Num; Not; Sign ]
        
        match m with
        | Data | FuncDecl -> Leaf
        | Epsilon | Id | Idi | Num | RelOp | Type -> Leaf
        | ClassDeclList -> Single (Many ClassDeclList)
        | FuncDefList -> Single (Many FuncDef)
        | Prog -> List [ One ClassDeclList; One FuncDefList; One MainFuncBody ]
        | MemberList -> Single (ManyOf [ VarDecl; FuncDecl ])
        | ClassDecl -> List [ One Type; One Id; One FParamList ]
        | FuncDef -> List [ One Type; OneOf [ Type; Epsilon ]; One Id; One FParamList; One FuncBody ]
        | MainFuncBody -> Single (ManyOf [ VarDecl; AssignStat; IfStat; WhileStat; ReadStat; WriteStat; ReturnStat ])
        | FuncBody -> Single (ManyOf [ VarDecl; AssignStat; IfStat; WhileStat; ReadStat; WriteStat; ReturnStat ])
        | StatBlock -> Single (ManyOf [ AssignStat; IfStat; WhileStat; ReadStat; WriteStat; ReturnStat ])
        | VarDecl -> List [ One Type; One Id ]
        | AssignStat -> List [ One VarElementList; OneOf expr ]
        | IfStat -> List [ OneOf expr; One StatBlock; One StatBlock ]
        | WhileStat -> List [ OneOf expr; One RelExpr; One StatBlock ]
        | ReadStat -> Single (One VarElementList)
        | WriteStat -> Single (OneOf expr)
        | ReturnStat -> Single (OneOf expr)
        | IndexList -> Single (ManyOf expr)
        | RelExpr -> List [ OneOf expr; One RelOp; OneOf expr ]
        | AddOp -> List [ OneOf arithExpr; OneOf term ]
        | MultOp -> List [ OneOf term; OneOf factor ]
        | Not -> Single (OneOf factor)
        | Sign -> Single (OneOf factor)
        | VarElementList -> Single (ManyOf [ DataMember; FunctionCall ])
        | DataMember -> List [ One Id; One IndexList ]
        | FunctionCall -> List [ One Id; One AParamList ]
        | InheritList -> Single (Many Idi)
        | FParam -> List [ One Type; One Id ]
        | FParamList -> Single (ManyOf expr)
        | AParamList -> Single (ManyOf expr)
        | EndProgram -> Leaf
                
and NodeChildren =
    | Single of NodeChildrenGroup
    | List of NodeChildrenGroup list
    | Leaf
    
and NodeChildrenGroup =
    | One of NodeType
    | OneOf of NodeType list
    | Many of NodeType
    | ManyOf of NodeType list

and VariableType =
    | Integer
    | Float
    | Class of string

and VariableKind =
    | Inherited
    | ReturnVar
    | ReturnAddr
    | InstAddr
    | Param of string
    | Var of string
    | WhileVar of string
    | TempVar of int
    | LitVar of int
        
and NodeElement =
    {
        nodeType: NodeType
        token: Lexer.Token option
    }
    static member inline make() =
        {
            nodeType = NodeType.Data
            token = None
        }
        
    override m.ToString() =
        let asStringOrNull arg = if Option.isSome arg then arg.ToString() else "null" 
        "{ " + m.nodeType.ToString() + ": " + (asStringOrNull m.token) + " }"

and AbstractSyntaxTree =
    {
        mutable root: int option
        mutable nodes: Node<NodeElement> list
        mutable errors: ASTError list
    }
    static member inline make(): AbstractSyntaxTree =
        {
            root = None
            nodes = List.empty
            errors = List.empty
        }
        
    member m.nodeTypeAt (nodeIndex: int) =
        (m.nodeElementAt nodeIndex).nodeType
        
    member m.nodeElementAt (nodeId: int) =
        m.nodes.[nodeId].element
        
    member m.childrenAt (nodeId: int) =
        m.nodes.[nodeId].children
        
    member m.addLeftChild (parentId: int) (childId: int) =
        let mutable parentNode = m.nodes.[parentId]
        List.insert (childId) 0 (&parentNode.children)
        let childNode = m.nodes.[childId]
        childNode.parent <- Some parentId
        
    member m.addOne (newNodeId: int) (nodeType: NodeType) (topNodeId: int) =
        if m.nodeTypeAt topNodeId = nodeType then
            m.addLeftChild newNodeId topNodeId
            true
        else
            false
            
    member m.addOneOf (newNodeId: int) (nodeList: NodeType list) (topNodeId: int) =
        if List.contains (m.nodeTypeAt topNodeId) nodeList then
            m.addLeftChild newNodeId topNodeId
            true
        else
            false
         
    member m.tryAddNodeChildren (semanticStack: int list byref, newNodeType: NodeType, newNodeId: int, nodeChildrenGroup: NodeChildrenGroup) =
        let mutable result: Result<unit, ASTError> = Ok ()
        
        match nodeChildrenGroup with
        | One nodeType ->
            match List.pop &semanticStack with
            | Some topNodeId ->
                if not (m.addOne newNodeId nodeType topNodeId) then
                    result <- Error (WrongNodeOnStackOne (newNodeType, nodeType, m.nodeTypeAt topNodeId))
            | None -> result <- Error (NoNodeOnStackOne (newNodeType, nodeType))
        | OneOf nodeList ->
            match List.pop &semanticStack with
            | Some topNodeId ->
                if not (m.addOneOf newNodeId nodeList topNodeId) then
                    result <- Error (WrongNodeOnStackList (newNodeType, nodeList, m.nodeTypeAt topNodeId))
            | None -> result <- Error (NoNodeOnStackList (newNodeType, nodeList))
        | Many nodeType ->
            let mutable keepGoing = true
            while keepGoing do
                match List.pop &semanticStack with
                | Some topNodeId -> 
                    if not (m.addOne newNodeId nodeType topNodeId) then
                        semanticStack <- semanticStack @ [ topNodeId ]
                        keepGoing <- false
                | None -> keepGoing <- false
        | ManyOf nodeList ->
            let mutable keepGoing = true
            while keepGoing do
                match List.pop &semanticStack with
                | Some topNodeId -> 
                    if not (m.addOneOf newNodeId nodeList topNodeId) then
                        semanticStack <- semanticStack @ [ topNodeId ]
                        keepGoing <- false
                | None -> keepGoing <- false
        
        result
       
    member m.makeNode (semanticStack: int list byref, tokenStack: Lexer.Token list byref, newNodeType: NodeType) =
        let mutable result: Result<unit, ASTError> = Ok ()
        let mutable tokenMaybe = None
        
        if newNodeType.needData then
            match List.pop &tokenStack with
            | Some token -> tokenMaybe <- Some token
            | None -> result <- Error (EmptyTokenStack newNodeType)
        
        match result with
        | Ok _ ->
            let newNodeId = m.newNode ({
                nodeType = newNodeType
                token = tokenMaybe
            })
            let nodeChildren = newNodeType.children
            match nodeChildren with
            | Single nodeChildrenGroup ->
                match m.tryAddNodeChildren (ref semanticStack, newNodeType, newNodeId, nodeChildrenGroup) with
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
                        match m.tryAddNodeChildren (ref semanticStack, newNodeType, newNodeId, nodeChildrenGroup) with
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
        
    member m.newNode (nodeElement: NodeElement) =
        let index = List.length m.nodes
        let node =
            {
                index = index
                parent = None
                children = List.empty
                element = nodeElement
            }
        m.nodes <- m.nodes @ [ node ]
        index

and AST = AbstractSyntaxTree

and ASTError =
    | EmptyTokenStack of NodeType
    | NoNodeOnStackOne of NodeType * NodeType
    | NoNodeOnStackList of NodeType * NodeType list
    | WrongNodeOnStackOne of NodeType * NodeType * NodeType
    | WrongNodeOnStackList of NodeType * NodeType list * NodeType
    override m.ToString() =
        match m with
        | EmptyTokenStack nodeType ->
            "EmptyTokenStack (No token found on token stack to make node: " + nodeType.ToString() + ")" 
        | NoNodeOnStackOne (nodeType, childNodeType) ->
            "NoNodeOnStackOne (Trying to make node " + nodeType.ToString() + " while expecting " + childNodeType.ToString() + ", but the stack is empty)" 
        | NoNodeOnStackList (nodeType, nodeList) ->
            "NoNodeOnStackList (Trying to make node " + nodeType.ToString() + ", while expecting any of [" + (List.map (fun e -> e.ToString()) nodeList |> String.concat ", ") + "], but the stack is empty)"
        | WrongNodeOnStackOne (nodeType, childNodeType, stackTopNodeType) ->
            "WrongNodeOnStackOne (Trying to make node " + nodeType.ToString() + " while expecting " + childNodeType.ToString() + ", but found node " + stackTopNodeType.ToString() + ")" 
        | WrongNodeOnStackList (nodeType, nodeList, stackTopNodeType) ->
            "WrongNodeOnStackList (Trying to make node " + nodeType.ToString() + ", while expecting any of [" + (List.map (fun e -> e.ToString()) nodeList |> String.concat ", ") + "], but found node " + stackTopNodeType.ToString() + ")" 
        
let asGraphViz (ast: AST): string =
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
        str <- str + "    node [label=\"" + nodeIndex.ToString() + ", " + (ast.nodeElementAt nodeIndex).ToString() + "\"] " + nodeIndex.ToString() + "\n"
        for childIndex in ast.childrenAt nodeIndex do
            asGraphVizNode childIndex
    
    let rec asGraphVizEdge (nodeIndex: int) =
        for childIndex in ast.childrenAt nodeIndex do
            str <- str + "    " + nodeIndex.ToString() + " -> " + childIndex.ToString() + "\n"
            asGraphVizEdge childIndex

    match ast.root with
    | Some rootIndex ->
        asGraphVizNode rootIndex
        asGraphVizEdge rootIndex
        str <- str + "}\n"
        str <- str.Replace(" ||", " \\|\\|")
        str <- str.Replace(" <", " \\<")
        str <- str.Replace(" >", " \\>")
        str <- str.Replace("{ ", "\\{ ")
        str <- str.Replace(" }", " \\}")
    | None -> result <- Error "AST has no root!"
    
    str
