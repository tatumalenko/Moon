namespace Moon

type NonTerminal =
    | Start
    | Prog
    | ClassDecl
    | ClassDeclList
    | FuncDecl
    | FuncHead
    | ScopeOp
    | FuncDef
    | FuncDefList
    | LocalVarDecl
    | VarDeclList
    | FuncBody
    | FuncBody'
    | Visibility
    | VarDecl
    | VarOrFuncDecl
    | VarOrFuncDecl'
    | MemberDecl
    | MemberDeclList
    | AssignStat
    | Statement
    | StatOrAssignOrFuncCall
    | StatOrAssignOrFuncCall'
    | StatOrAssignOrFuncCallList
    | StatBlock
    | Expr
    | Expr'
    | RelExpr
    | ArithExpr
    | ArithExpr'
    | Sign
    | Term
    | Term'
    | Factor
    | Factor'
    | Variable
    | IdNest
    | IdNest'
    | IdNestRep
    | InherListOp
    | InherList
    | Index
    | IndexList
    | Dim
    | Dim'
    | DimList
    | DimList'
    | DimListOrEpsilon
    | Type
    | TypeOrVoid
    | FParam
    | FParamList
    | FParamList'
    | AParam
    | AParamList
    | AParamList'
    | AssignOp
    | RelOp
    | AddOp
    | MultOp

    static member fromString (text: string) =
        match text with
        | "start" -> Ok Start
        | "prog" -> Ok Prog
        | "classDecl" -> Ok ClassDecl
        | "classDeclList" -> Ok ClassDeclList
        | "funcDecl" -> Ok FuncDecl
        | "funcHead" -> Ok FuncHead
        | "scopeOp" -> Ok ScopeOp
        | "funcDef" -> Ok FuncDef
        | "funcDefList" -> Ok FuncDefList
        | "localVarDecl" -> Ok LocalVarDecl
        | "varDeclList" -> Ok VarDeclList
        | "funcBody" -> Ok FuncBody
        | "funcBody'" -> Ok FuncBody'
        | "visibility" -> Ok Visibility
        | "varDecl" -> Ok VarDecl
        | "varOrFuncDecl" -> Ok VarOrFuncDecl
        | "varOrFuncDecl'" -> Ok VarOrFuncDecl'
        | "memberDecl" -> Ok MemberDecl
        | "memberDeclList" -> Ok MemberDeclList
        | "assignStat" -> Ok AssignStat
        | "statement" -> Ok Statement
        | "statOrAssignOrFuncCall" -> Ok StatOrAssignOrFuncCall
        | "statOrAssignOrFuncCall'" -> Ok StatOrAssignOrFuncCall'
        | "statOrAssignOrFuncCallList" -> Ok StatOrAssignOrFuncCallList
        | "statBlock" -> Ok StatBlock
        | "expr" -> Ok Expr
        | "expr'" -> Ok Expr'
        | "relExpr" -> Ok RelExpr
        | "arithExpr" -> Ok ArithExpr
        | "arithExpr'" -> Ok ArithExpr'
        | "sign" -> Ok Sign
        | "term" -> Ok Term
        | "term'" -> Ok Term'
        | "factor" -> Ok Factor
        | "factor'" -> Ok Factor'
        | "variable" -> Ok Variable
        | "idnest" -> Ok IdNest
        | "idnest'" -> Ok IdNest'
        | "idnestRep" -> Ok IdNestRep
        | "inherListOp" -> Ok InherListOp
        | "inherList" -> Ok InherList
        | "index" -> Ok Index
        | "indexList" -> Ok IndexList
        | "dim" -> Ok Dim
        | "dim'" -> Ok Dim'
        | "dimList" -> Ok DimList
        | "dimList'" -> Ok DimList'
        | "dimListOrEpsilon" -> Ok DimListOrEpsilon
        | "type" -> Ok Type
        | "typeOrVoid" -> Ok TypeOrVoid
        | "fParam" -> Ok FParam
        | "fParamList" -> Ok FParamList
        | "fParamList'" -> Ok FParamList'
        | "aParam" -> Ok AParam
        | "aParamList" -> Ok AParamList
        | "aParamList'" -> Ok AParamList'
        | "assignOp" -> Ok AssignOp
        | "relOp" -> Ok RelOp
        | "addOp" -> Ok AddOp
        | "multOp" -> Ok MultOp
        | symbol -> Error("Invalid grammar symbol: " + symbol)
