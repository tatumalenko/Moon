module Moon.Language

type Variable =
    | Start
    | Prog
    | Prog1
    | Prog2
    | ClassDecl
    | ClassDecl1
    | ClassDecl2
    | ClassDecl3
    | Visibility
    | MemberDecl
    | MemberDecl1
    | FuncDecl
    | FuncDecl1
    | FuncHead
    | FuncHead1
    | FuncDef
    | FuncDeclIdless
    | FuncBody
    | FuncBody1
    | FuncBody2
    | FuncBody3
    | FuncCall
    | VarDecl
    | VarDecl1
    | VarDecl2
    | Stat
    | Stat1
    | Stat2
    | Stat3
    | Stat4
    | Stat5
    | Stat6
    | AssignStat
    | StatBlock
    | StatBlock1
    | Expr
    | Expr1
    | RelExpr
    | ArithExpr
    | ArithExpr1
    | Sign
    | Term
    | Term1
    | Factor
    | Factor1
    | Factor2
    | Factor3
    | Factor4
    | Factor5
    | Var
    | Var1
    | Var2
    | Var3
    | Var4
    | Var5
    | IdNest
    | IdNest1
    | IdNest2
    | IdNest3
    | Index
    | ArraySize
    | Type
    | TypeOrVoid
    | FParams
    | FParams1
    | FParams2
    | AParams
    | AParams1
    | FParamsTail
    | FParamsTail1
    | AParamsTail
    | AssignOp
    | RelOp
    | AddOp
    | MultOp
    | IntegerOrFloat
    | MaybeIntNum
    | E | T | X | Y | Z
    static member fromString (text: string) =
        match text with
        | "START" -> Ok Start
        | "PROG" -> Ok Prog
        | "PROG_1" -> Ok Prog1
        | "PROG_2" -> Ok Prog2
        | "CLASSDECL" -> Ok ClassDecl
        | "CLASSDECL_1" -> Ok ClassDecl1
        | "CLASSDECL_2" -> Ok ClassDecl2
        | "CLASSDECL_3" -> Ok ClassDecl3
        | "VISIBILITY" -> Ok Visibility
        | "MEMBERDECL" -> Ok MemberDecl
        | "MEMBERDECL_1" -> Ok MemberDecl1
        | "FUNCDECL" -> Ok FuncDecl
        | "FUNCDECL_1" -> Ok FuncDecl1
        | "FUNCHEAD" -> Ok FuncHead
        | "FUNCHEAD_1" -> Ok FuncHead1
        | "FUNCDEF" -> Ok FuncDef
        | "FUNCDECLIDLESS" -> Ok FuncDeclIdless
        | "FUNCBODY" -> Ok FuncBody
        | "FUNCBODY_1" -> Ok FuncBody1
        | "FUNCBODY_2" -> Ok FuncBody2
        | "FUNCBODY_3" -> Ok FuncBody3
        | "VARDECL" -> Ok VarDecl
        | "VARDECL_1" -> Ok VarDecl1
        | "VARDECL_2" -> Ok VarDecl2
        | "STATEMENT" -> Ok Stat
        | "STATEMENT_1" -> Ok Stat1
        | "STATEMENT_2" -> Ok Stat2
        | "STATEMENT_3" -> Ok Stat3
        | "STATEMENT_4" -> Ok Stat4
        | "STATEMENT_5" -> Ok Stat5
        | "STATEMENT_6" -> Ok Stat6
        | "ASSIGNSTAT" -> Ok AssignStat
        | "STATBLOCK" -> Ok StatBlock
        | "STATBLOCK_1" -> Ok StatBlock1
        | "EXPR" -> Ok Expr
        | "EXPR_1" -> Ok Expr1
        | "RELEXPR" -> Ok RelExpr
        | "ARITHEXPR" -> Ok ArithExpr
        | "ARITHEXPR_1" -> Ok ArithExpr1
        | "SIGN" -> Ok Sign
        | "TERM" -> Ok Term
        | "TERM_1" -> Ok Term1
        | "FACTOR" -> Ok Factor
        | "FACTOR_1" -> Ok Factor1
        | "FACTOR_2" -> Ok Factor2
        | "FACTOR_3" -> Ok Factor3
        | "FACTOR_4" -> Ok Factor4
        | "FACTOR_5" -> Ok Factor5
        | "VARIABLE" -> Ok Var
        | "VARIABLE_1" -> Ok Var1
        | "VARIABLE_2" -> Ok Var2
        | "VARIABLE_3" -> Ok Var3
        | "VARIABLE_4" -> Ok Var4
        | "VARIABLE_5" -> Ok Var5
        | "FUNCTIONCALL" -> Ok FuncCall
        | "IDNEST" -> Ok IdNest
        | "IDNEST_1" -> Ok IdNest1
        | "IDNEST_2" -> Ok IdNest2
        | "IDNEST_3" -> Ok IdNest3
        | "INDEX" -> Ok Index
        | "ARRAYSIZE" -> Ok ArraySize
        | "TYPE" -> Ok Type
        | "TYPEORVOID" -> Ok TypeOrVoid
        | "FPARAMS" -> Ok FParams
        | "FPARAMS_1" -> Ok FParams1
        | "FPARAMS_2" -> Ok FParams2
        | "APARAMS" -> Ok AParams
        | "APARAMS_1" -> Ok AParams1
        | "FPARAMSTAIL" -> Ok FParamsTail
        | "FPARAMSTAIL_1" -> Ok FParamsTail1
        | "APARAMSTAIL" -> Ok AParamsTail
        | "ASSIGNOP" -> Ok AssignOp
        | "RELOP" -> Ok RelOp
        | "ADDOP" -> Ok AddOp
        | "MULTOP" -> Ok MultOp
        | "INTEGERORFLOAT" -> Ok IntegerOrFloat
        | "MAYBEINTNUM" -> Ok MaybeIntNum
        | symbol -> Error ("Invalid grammar symbol: " + symbol) 
    