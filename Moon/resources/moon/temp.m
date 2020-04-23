          entry                         
          addi      r14,r0,topaddr      
          lw        r1,t2(r0)           % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList [ DataMember [ Id (undeclVar) IndexList ]  ]  ] 
          sw        t1(r0),r1           
          lw        r1,t3(r0)           % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList [ DataMember, Variable a: A [ Id (a) IndexList ]  DataMember [ Id (undeclVar)
          sw        t1(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t4(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t5(r0),r1           
          lw        r1,t5(r0)           % Plus (+), Variable +: integer [ IntegerLiteral (3), Variable 3: integer IntegerLiteral (3), Variable 3: integer ] 
          lw        r2,t4(r0)           
          add       r3,r2,r1            
          sw        t6(r0),r3           
          lw        r1,t6(r0)           % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  Plus (+), Variable +: integer [ IntegerLiteral (3), Variable 3: integer IntegerLiteral (3), 
          sw        t1(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t7(r0),r1           
          lw        r1,t8(r0)           % Plus (+), Variable +: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
          lw        r2,t7(r0)           
          add       r3,r2,r1            
          sw        t9(r0),r3           
          lw        r1,t9(r0)           % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  Plus (+), Variable +: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Vari
          sw        t1(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t10(r0),r1          
          lw        r1,t8(r0)           % Asterisk (*), Variable *: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
          lw        r2,t10(r0)          
          mul       r3,r2,r1            
          sw        t11(r0),r3          
          lw        r1,t11(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  Asterisk (*), Variable *: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, 
          sw        t1(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t12(r0),r1          
          lw        r1,t12(r0)          % Or (or), Variable or: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
          lw        r2,t8(r0)           
          bnz       r1,l1               
          bnz       r2,l1               
          addi      r3,r0,0             
          j         l2                  
l1        addi      r3,r0,1             
l2        sw        t13(r0),r3          
          lw        r1,t13(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  Or (or), Variable or: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Vari
          sw        t1(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t14(r0),r1          
          lw        r1,t14(r0)          % And (and), Variable and: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
          lw        r2,t8(r0)           
          bz        r1,l3               
          bz        r2,l3               
          addi      r3,r0,1             
          j         l4                  
l3        addi      r3,r0,0             
l4        sw        t15(r0),r3          
          lw        r1,t15(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  And (and), Variable and: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, V
          sw        t1(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t16(r0),r1          
          lw        r1,t16(r0)          % RelExpr, Variable 3: integer [ IntegerLiteral (3), Variable 3: integer Gt (>) VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
          lw        r2,t8(r0)           
          cgt       r3,r1,r2            
          sw        t17(r0),r3          
          lw        r1,t17(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  RelExpr, Variable 3: integer [ IntegerLiteral (3), Variable 3: integer Gt (>) VarElementList
          sw        t1(r0),r1           
          lw        r1,t18(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Function sum(integer[-1], integer): integer [ DataMember, Variable a: A [ Id
          sw        t1(r0),r1           
          lw        r1,t18(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Function sum(integer[-1], integer): integer [ DataMember, Variable a: A [ Id
          sw        t1(r0),r1           
          lw        r1,t18(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Function sum(integer[-1], integer): integer [ DataMember, Variable a: A [ Id
          sw        t1(r0),r1           
          lw        r1,t19(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList [ DataMember, Variable a: A [ Id (a) IndexList ]  FunctionCall [ Id (undecl) 
          sw        t1(r0),r1           
          lw        r1,t18(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Function sum(integer[-1], integer): integer [ DataMember, Variable a: A [ Id
          sw        t1(r0),r1           
          lw        r1,t21(r0)          % AssignStat [ VarElementList, Variable f: float [ DataMember, Variable f: float [ Id (f) IndexList ]  ]  VarElementList [ DataMember, Variable arr1d: integer[7] [ Id (arr1d) IndexList ]  DataMember [ I
          sw        t20(r0),r1          
          lw        r1,t22(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr1d: integer [ DataMember, Variable arr1d: integer [ Id (arr1d) P
          sw        t1(r0),r1           
          lw        r1,t22(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr1d: integer [ DataMember, Variable arr1d: integer [ Id (arr1d) A
          sw        t1(r0),r1           
          lw        r1,t22(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr1d: integer [ DataMember, Variable arr1d: integer [ Id (arr1d) O
          sw        t1(r0),r1           
          lw        r1,t22(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr1d: integer [ DataMember, Variable arr1d: integer [ Id (arr1d) V
          sw        t1(r0),r1           
          lw        r1,t22(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr1d: integer [ DataMember, Variable arr1d: integer [ Id (arr1d) V
          sw        t1(r0),r1           
          lw        r1,t23(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList [ DataMember [ Id (arr1d) IndexList [ IntegerLiteral (1), Variable 1: integer
          sw        t1(r0),r1           
          lw        r1,t22(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr1d: integer [ DataMember, Variable arr1d: integer [ Id (arr1d) V
          sw        t1(r0),r1           
          lw        r1,t24(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr2d: integer [ DataMember, Variable arr2d: integer [ Id (arr2d) P
          sw        t1(r0),r1           
          lw        r1,t24(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr2d: integer [ DataMember, Variable arr2d: integer [ Id (arr2d) A
          sw        t1(r0),r1           
          lw        r1,t24(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr2d: integer [ DataMember, Variable arr2d: integer [ Id (arr2d) O
          sw        t1(r0),r1           
          lw        r1,t24(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr2d: integer [ DataMember, Variable arr2d: integer [ Id (arr2d) V
          sw        t1(r0),r1           
          lw        r1,t24(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr2d: integer [ DataMember, Variable arr2d: integer [ Id (arr2d) V
          sw        t1(r0),r1           
          lw        r1,t25(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList [ DataMember [ Id (arr2d) IndexList [ IntegerLiteral (1), Variable 1: integer
          sw        t1(r0),r1           
          lw        r1,t24(r0)          % AssignStat [ VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ]  VarElementList, Variable arr2d: integer [ DataMember, Variable arr2d: integer [ Id (arr2d) V
          sw        t1(r0),r1           
          lw        r1,t18(r0)          % AssignStat [ VarElementList, Variable f: float [ DataMember, Variable f: float [ Id (f) IndexList ]  ]  VarElementList, Function sum(integer[-1], integer): integer [ DataMember, Variable a: A [ Id (a)
          sw        t20(r0),r1          
          lw        r1,t26(r0)          % AssignStat [ VarElementList, Variable f: float [ DataMember, Variable f: float [ Id (f) IndexList ]  ]  VarElementList, Function A.randFloat(): float [ DataMember, Variable a: A [ Id (a) IndexList ]  
          sw        t20(r0),r1          
          lw        r1,t27(r0)          % AssignStat [ VarElementList, Variable f: float [ DataMember, Variable f: float [ Id (f) IndexList ]  ]  VarElementList, Function A.randFloat2(): float [ DataMember, Variable a: A [ Id (a) IndexList ] 
          sw        t20(r0),r1          
          hlt                           

t1        res       4    % VarElementList, Variable i: integer [ DataMember, Variable i: integer [ Id (i) IndexList ]  ] 
t2        res       0    % VarElementList [ DataMember [ Id (undeclVar) IndexList ]  ] 
t3        res       0    % VarElementList [ DataMember, Variable a: A [ Id (a) IndexList ]  DataMember [ Id (undeclVar) IndexList ]  ] 
t4        res       4    % IntegerLiteral (3), Variable 3: integer
t5        res       4    % IntegerLiteral (3), Variable 3: integer
t6        res       4    % Plus (+), Variable +: integer [ IntegerLiteral (3), Variable 3: integer IntegerLiteral (3), Variable 3: integer ] 
t7        res       4    % IntegerLiteral (3), Variable 3: integer
t8        res       20   % VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ] 
t9        res       4    % Plus (+), Variable +: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
t10       res       4    % IntegerLiteral (3), Variable 3: integer
t11       res       4    % Asterisk (*), Variable *: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
t12       res       4    % IntegerLiteral (3), Variable 3: integer
t13       res       4    % Or (or), Variable or: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
t14       res       4    % IntegerLiteral (3), Variable 3: integer
t15       res       4    % And (and), Variable and: integer [ IntegerLiteral (3), Variable 3: integer VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
t16       res       4    % IntegerLiteral (3), Variable 3: integer
t17       res       4    % RelExpr, Variable 3: integer [ IntegerLiteral (3), Variable 3: integer Gt (>) VarElementList, Variable a: A [ DataMember, Variable a: A [ Id (a) IndexList ]  ]  ] 
t18       res       0    % VarElementList, Function sum(integer[-1], integer): integer [ DataMember, Variable a: A [ Id (a) IndexList ]  FunctionCall, Function sum(integer[-1], integer): integer [ Id (sum) AParamList ]  ] 
t19       res       0    % VarElementList [ DataMember, Variable a: A [ Id (a) IndexList ]  FunctionCall [ Id (undecl) AParamList ]  ] 
t20       res       8    % VarElementList, Variable f: float [ DataMember, Variable f: float [ Id (f) IndexList ]  ] 
t21       res       0    % VarElementList [ DataMember, Variable arr1d: integer[7] [ Id (arr1d) IndexList ]  DataMember [ Id (undecl) IndexList ]  ] 
t22       res       4    % VarElementList, Variable arr1d: integer [ DataMember, Variable arr1d: integer [ Id (arr1d) Plus (+), Variable +: integer [ IntegerLiteral (1), Variable 1: integer IntegerLiteral (3), Variable 3: integ
t23       res       0    % VarElementList [ DataMember [ Id (arr1d) IndexList [ IntegerLiteral (1), Variable 1: integer IntegerLiteral (1), Variable 1: integer ]  ]  ] 
t24       res       4    % VarElementList, Variable arr2d: integer [ DataMember, Variable arr2d: integer [ Id (arr2d) Plus (+), Variable +: integer [ IntegerLiteral (1), Variable 1: integer IntegerLiteral (3), Variable 3: integ
t25       res       0    % VarElementList [ DataMember [ Id (arr2d) IndexList [ IntegerLiteral (1), Variable 1: integer ]  ]  ] 
t26       res       0    % VarElementList, Function A.randFloat(): float [ DataMember, Variable a: A [ Id (a) IndexList ]  FunctionCall, Function A.randFloat(): float [ Id (randFloat) AParamList ]  ] 
t27       res       0    % VarElementList, Function A.randFloat2(): float [ DataMember, Variable a: A [ Id (a) IndexList ]  FunctionCall, Function A.randFloat2(): float [ Id (randFloat2) AParamList ]  ] 
buf       res       20   % Buffer space used for console output
space     db        " "  % Separator for console output