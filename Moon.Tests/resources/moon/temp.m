          entry                         
          addi      r14,r0,topaddr      
          addi      r1,r0,35            % IntegerLiteral (35), Variable 35: integer
          sw        t2(r0),r1           
          lw        r1,t2(r0)           % AssignStat [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  IntegerLiteral (35), Variable 35: integer ] 
          sw        t1(r0),r1           
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t4(r0),r1           
          lw        r1,t4(r0)           % AssignStat [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  IntegerLiteral (3), Variable 3: integer ] 
          sw        t3(r0),r1           
          addi      r1,r0,2             % IntegerLiteral (2), Variable 2: integer
          sw        t6(r0),r1           
          lw        r1,t6(r0)           % AssignStat [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  IntegerLiteral (2), Variable 2: integer ] 
          sw        t5(r0),r1           
          addi      r1,r0,4             % IntegerLiteral (4), Variable 4: integer
          sw        t8(r0),r1           
          lw        r1,t8(r0)           % AssignStat [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  IntegerLiteral (4), Variable 4: integer ] 
          sw        t7(r0),r1           
          addi      r1,r0,5             % IntegerLiteral (5), Variable 5: integer
          sw        t10(r0),r1          
          lw        r1,t10(r0)          % AssignStat [ VarElementList, Variable b: integer [ DataMember, Variable b: integer [ Id (b) IndexList ]  ]  IntegerLiteral (5), Variable 5: integer ] 
          sw        t9(r0),r1           
          lw        r1,t3(r0)           % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ I
          lw        r2,t1(r0)           
          add       r3,r2,r1            
          sw        t11(r0),r3          
          lw        r1,t11(r0)          % WriteStat [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y:
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t5(r0)           % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable z: integer [ DataMember, Variable z: integer [ 
          lw        r2,t3(r0)           
          sub       r3,r2,r1            
          sw        t12(r0),r3          
          lw        r1,t12(r0)          % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
          lw        r2,t1(r0)           
          add       r3,r2,r1            
          sw        t13(r0),r3          
          lw        r1,t13(r0)          % WriteStat [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t7(r0)           % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer [ DataMember, Variable a: integer [ 
          lw        r2,t5(r0)           
          div       r3,r2,r1            
          sw        t14(r0),r3          
          lw        r1,t14(r0)          % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ D
          lw        r2,t3(r0)           
          sub       r3,r2,r1            
          sw        t15(r0),r3          
          lw        r1,t15(r0)          % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
          lw        r2,t1(r0)           
          add       r3,r2,r1            
          sw        t16(r0),r3          
          lw        r1,t16(r0)          % WriteStat [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t9(r0)           % Slash (/), Variable /: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable b: integer [ DataMember, Variable b: integer [ 
          lw        r2,t1(r0)           
          div       r3,r2,r1            
          sw        t17(r0),r3          
          lw        r1,t17(r0)          % WriteStat [ Slash (/), Variable /: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable b: integer [ DataMember, Variable b
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t9(r0)           % Asterisk (*), Variable *: integer [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  VarElementList, Variable b: integer [ DataMember, Variable b: integer
          lw        r2,t7(r0)           
          mul       r3,r2,r1            
          sw        t18(r0),r3          
          lw        r1,t18(r0)          % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  Asterisk (*), Variable *: integer [ VarElementList, Variable a: integer 
          lw        r2,t5(r0)           
          div       r3,r2,r1            
          sw        t19(r0),r3          
          lw        r1,t19(r0)          % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ D
          lw        r2,t3(r0)           
          sub       r3,r2,r1            
          sw        t20(r0),r3          
          lw        r1,t20(r0)          % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
          lw        r2,t1(r0)           
          add       r3,r2,r1            
          sw        t21(r0),r3          
          lw        r1,t21(r0)          % WriteStat [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t7(r0)           % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer [ DataMember, Variable a: integer [ 
          lw        r2,t5(r0)           
          div       r3,r2,r1            
          sw        t22(r0),r3          
          lw        r1,t22(r0)          % WriteStat [ Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer [ DataMember, Variable a
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t5(r0)           % Slash (/), Variable /: integer [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  VarElementList, Variable z: integer [ DataMember, Variable z: integer [ 
          lw        r2,t7(r0)           
          div       r3,r2,r1            
          sw        t23(r0),r3          
          lw        r1,t23(r0)          % WriteStat [ Slash (/), Variable /: integer [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  VarElementList, Variable z: integer [ DataMember, Variable z
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t7(r0)           % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer [ DataMember, Variable a: integer [ 
          lw        r2,t5(r0)           
          div       r3,r2,r1            
          sw        t24(r0),r3          
          lw        r1,t9(r0)           % Asterisk (*), Variable *: integer [ Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer 
          lw        r2,t24(r0)          
          mul       r3,r2,r1            
          sw        t25(r0),r3          
          lw        r1,t25(r0)          % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  Asterisk (*), Variable *: integer [ Slash (/), Variable /: integer [ Var
          lw        r2,t3(r0)           
          sub       r3,r2,r1            
          sw        t26(r0),r3          
          lw        r1,t26(r0)          % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
          lw        r2,t1(r0)           
          add       r3,r2,r1            
          sw        t27(r0),r3          
          lw        r1,t27(r0)          % WriteStat [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          addi      r1,r0,49            % IntegerLiteral (49), Variable 49: integer
          sw        t28(r0),r1          
          lw        r1,t28(r0)          % Asterisk (*), Variable *: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  IntegerLiteral (49), Variable 49: integer ] 
          lw        r2,t3(r0)           
          mul       r3,r2,r1            
          sw        t29(r0),r3          
          lw        r1,t5(r0)           % Minus (-), Variable -: integer [ Asterisk (*), Variable *: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  IntegerLiteral (49), Variable 49: in
          lw        r2,t29(r0)          
          sub       r3,r2,r1            
          sw        t30(r0),r3          
          lw        r1,t30(r0)          % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ Asterisk (*), Variable *: integer [ VarE
          lw        r2,t1(r0)           
          add       r3,r2,r1            
          sw        t31(r0),r3          
          lw        r1,t9(r0)           % Asterisk (*), Variable *: integer [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  VarElementList, Variable b: integer [ DataMember, Variable b: integer
          lw        r2,t7(r0)           
          mul       r3,r2,r1            
          sw        t32(r0),r3          
          lw        r1,t32(r0)          % Slash (/), Variable /: integer [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ Asteris
          lw        r2,t31(r0)          
          div       r3,r2,r1            
          sw        t33(r0),r3          
          lw        r1,t33(r0)          % WriteStat [ Slash (/), Variable /: integer [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integ
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          addi      r1,r0,0             % IntegerLiteral (0), Variable 0: integer
          sw        t35(r0),r1          
          lw        r1,t35(r0)          % AssignStat [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  IntegerLiteral (0), Variable 0: integer ] 
          sw        t34(r0),r1          
          addi      r1,r0,1             % IntegerLiteral (1), Variable 1: integer
          sw        t37(r0),r1          
          lw        r1,t37(r0)          % AssignStat [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  IntegerLiteral (1), Variable 1: integer ] 
          sw        t36(r0),r1          
          addi      r1,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t38(r0),r1          
          lw        r1,t38(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r2,r0,r1            
          sw        t39(r0),r2          
          lw        r1,t39(r0)          % AssignStat [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ]  ] 
          sw        t1(r0),r1           
          addi      r1,r0,145           % IntegerLiteral (145), Variable 145: integer
          sw        t40(r0),r1          
          lw        r1,t40(r0)          % AssignStat [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  IntegerLiteral (145), Variable 145: integer ] 
          sw        t3(r0),r1           
          lw        r1,t1(r0)           % WriteStat [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  ] 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % WriteStat [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ] 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ] 
          sub       r2,r0,r1            
          sw        t41(r0),r2          
          lw        r1,t41(r0)          % WriteStat [ Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ]  ] 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t36(r0)          % And (and), Variable and: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable one
          lw        r2,t36(r0)          
          bz        r1,l1               
          bz        r2,l1               
          addi      r3,r0,1             
          j         l2                  
l1        addi      r3,r0,0             
l2        sw        t42(r0),r3          
          lw        r1,t42(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable 
          lw        r2,t36(r0)          
          bz        r1,l3               
          bz        r2,l3               
          addi      r3,r0,1             
          j         l4                  
l3        addi      r3,r0,0             
l4        sw        t43(r0),r3          
          lw        r1,t43(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable one: integer [ DataMembe
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t36(r0)          % And (and), Variable and: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable ze
          lw        r2,t34(r0)          
          bz        r1,l5               
          bz        r2,l5               
          addi      r3,r0,1             
          j         l6                  
l5        addi      r3,r0,0             
l6        sw        t44(r0),r3          
          lw        r1,t44(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember,
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable
          lw        r2,t34(r0)          
          bz        r1,l7               
          bz        r2,l7               
          addi      r3,r0,1             
          j         l8                  
l7        addi      r3,r0,0             
l8        sw        t45(r0),r3          
          lw        r1,t45(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMemb
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t1(r0)           % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
          lw        r2,t3(r0)           
          bz        r1,l9               
          bz        r2,l9               
          addi      r3,r0,1             
          j         l10                 
l9        addi      r3,r0,0             
l10       sw        t46(r0),r3          
          lw        r1,t46(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer 
          lw        r2,t1(r0)           
          bz        r1,l11              
          bz        r2,l11              
          addi      r3,r0,1             
          j         l12                 
l11       addi      r3,r0,0             
l12       sw        t47(r0),r3          
          lw        r1,t47(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t1(r0)           % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer 
          lw        r2,t1(r0)           
          bz        r1,l13              
          bz        r2,l13              
          addi      r3,r0,1             
          j         l14                 
l13       addi      r3,r0,0             
l14       sw        t48(r0),r3          
          lw        r1,t48(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
          lw        r2,t3(r0)           
          bz        r1,l15              
          bz        r2,l15              
          addi      r3,r0,1             
          j         l16                 
l15       addi      r3,r0,0             
l16       sw        t49(r0),r3          
          lw        r1,t49(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y:
          lw        r2,t3(r0)           
          bz        r1,l17              
          bz        r2,l17              
          addi      r3,r0,1             
          j         l18                 
l17       addi      r3,r0,0             
l18       sw        t50(r0),r3          
          lw        r1,t50(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember,
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: in
          lw        r2,t34(r0)          
          bz        r1,l19              
          bz        r2,l19              
          addi      r3,r0,1             
          j         l20                 
l19       addi      r3,r0,0             
l20       sw        t51(r0),r3          
          lw        r1,t51(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Varia
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x:
          lw        r2,t1(r0)           
          bz        r1,l21              
          bz        r2,l21              
          addi      r3,r0,1             
          j         l22                 
l21       addi      r3,r0,0             
l22       sw        t52(r0),r3          
          lw        r1,t52(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember,
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t1(r0)           % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: in
          lw        r2,t34(r0)          
          bz        r1,l23              
          bz        r2,l23              
          addi      r3,r0,1             
          j         l24                 
l23       addi      r3,r0,0             
l24       sw        t53(r0),r3          
          lw        r1,t53(r0)          % WriteStat [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Varia
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable one
          lw        r2,t36(r0)          
          bnz       r1,l25              
          bnz       r2,l25              
          addi      r3,r0,0             
          j         l26                 
l25       addi      r3,r0,1             
l26       sw        t54(r0),r3          
          lw        r1,t54(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, 
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t36(r0)          % Or (or), Variable or: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero:
          lw        r2,t34(r0)          
          bnz       r1,l27              
          bnz       r2,l27              
          addi      r3,r0,0             
          j         l28                 
l27       addi      r3,r0,1             
l28       sw        t55(r0),r3          
          lw        r1,t55(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Va
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable ze
          lw        r2,t34(r0)          
          bnz       r1,l29              
          bnz       r2,l29              
          addi      r3,r0,0             
          j         l30                 
l29       addi      r3,r0,1             
l30       sw        t56(r0),r3          
          lw        r1,t56(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember,
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t36(r0)          % Or (or), Variable or: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable one: i
          lw        r2,t36(r0)          
          bnz       r1,l31              
          bnz       r2,l31              
          addi      r3,r0,0             
          j         l32                 
l31       addi      r3,r0,1             
l32       sw        t57(r0),r3          
          lw        r1,t57(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Var
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t1(r0)           % Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ I
          lw        r2,t3(r0)           
          bnz       r1,l33              
          bnz       r2,l33              
          addi      r3,r0,0             
          j         l34                 
l33       addi      r3,r0,1             
l34       sw        t58(r0),r3          
          lw        r1,t58(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y:
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer [ I
          lw        r2,t1(r0)           
          bnz       r1,l35              
          bnz       r2,l35              
          addi      r3,r0,0             
          j         l36                 
l35       addi      r3,r0,1             
l36       sw        t59(r0),r3          
          lw        r1,t59(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x:
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t1(r0)           % Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer [ I
          lw        r2,t1(r0)           
          bnz       r1,l37              
          bnz       r2,l37              
          addi      r3,r0,0             
          j         l38                 
l37       addi      r3,r0,1             
l38       sw        t60(r0),r3          
          lw        r1,t60(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x:
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ I
          lw        r2,t3(r0)           
          bnz       r1,l39              
          bnz       r2,l39              
          addi      r3,r0,0             
          j         l40                 
l39       addi      r3,r0,1             
l40       sw        t61(r0),r3          
          lw        r1,t61(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y:
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: in
          lw        r2,t3(r0)           
          bnz       r1,l41              
          bnz       r2,l41              
          addi      r3,r0,0             
          j         l42                 
l41       addi      r3,r0,1             
l42       sw        t62(r0),r3          
          lw        r1,t62(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Va
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t3(r0)           % Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: integ
          lw        r2,t34(r0)          
          bnz       r1,l43              
          bnz       r2,l43              
          addi      r3,r0,0             
          j         l44                 
l43       addi      r3,r0,1             
l44       sw        t63(r0),r3          
          lw        r1,t63(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: in
          lw        r2,t1(r0)           
          bnz       r1,l45              
          bnz       r2,l45              
          addi      r3,r0,0             
          j         l46                 
l45       addi      r3,r0,1             
l46       sw        t64(r0),r3          
          lw        r1,t64(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Va
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t1(r0)           % Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: integ
          lw        r2,t34(r0)          
          bnz       r1,l47              
          bnz       r2,l47              
          addi      r3,r0,0             
          j         l48                 
l47       addi      r3,r0,1             
l48       sw        t65(r0),r3          
          lw        r1,t65(r0)          % WriteStat [ Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable
          sw        -8(r14),r1          
          addi      r1,r0,buf           
          sw        -12(r14),r1         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r1,space(r0)        
          putc      r1                  
          lw        r1,t34(r0)          % Not (not), Variable not: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  ] 
          bnz       r1,l49              
          addi      r1,r0,1             
          sw        t66(r0),r1          
          j         l50                 
l49       sw        t66(r0),r0          
l50                                     
          lw        r2,t66(r0)          % WriteStat [ Not (not), Variable not: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  ]  ] 
          sw        -8(r14),r2          
          addi      r2,r0,buf           
          sw        -12(r14),r2         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r2,space(r0)        
          putc      r2                  
          lw        r2,t36(r0)          % Not (not), Variable not: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  ] 
          bnz       r2,l51              
          addi      r2,r0,1             
          sw        t67(r0),r2          
          j         l52                 
l51       sw        t67(r0),r0          
l52                                     
          lw        r3,t67(r0)          % WriteStat [ Not (not), Variable not: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  ]  ] 
          sw        -8(r14),r3          
          addi      r3,r0,buf           
          sw        -12(r14),r3         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r3,space(r0)        
          putc      r3                  
          lw        r3,t1(r0)           % Not (not), Variable not: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  ] 
          bnz       r3,l53              
          addi      r3,r0,1             
          sw        t68(r0),r3          
          j         l54                 
l53       sw        t68(r0),r0          
l54                                     
          lw        r4,t68(r0)          % WriteStat [ Not (not), Variable not: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  ]  ] 
          sw        -8(r14),r4          
          addi      r4,r0,buf           
          sw        -12(r14),r4         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r4,space(r0)        
          putc      r4                  
          lw        r4,t3(r0)           % Not (not), Variable not: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ] 
          bnz       r4,l55              
          addi      r4,r0,1             
          sw        t69(r0),r4          
          j         l56                 
l55       sw        t69(r0),r0          
l56                                     
          lw        r5,t69(r0)          % WriteStat [ Not (not), Variable not: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ]  ] 
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t34(r0)          % Minus (-), Variable -: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  ] 
          sub       r6,r0,r5            
          sw        t70(r0),r6          
          lw        r5,t70(r0)          % WriteStat [ Minus (-), Variable -: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  ]  ] 
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t36(r0)          % Minus (-), Variable -: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  ] 
          sub       r6,r0,r5            
          sw        t71(r0),r6          
          lw        r5,t71(r0)          % WriteStat [ Minus (-), Variable -: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  ]  ] 
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % Minus (-), Variable -: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  ] 
          sub       r6,r0,r5            
          sw        t72(r0),r6          
          lw        r5,t72(r0)          % WriteStat [ Minus (-), Variable -: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  ]  ] 
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t3(r0)           % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ] 
          sub       r6,r0,r5            
          sw        t73(r0),r6          
          lw        r5,t73(r0)          % WriteStat [ Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ]  ] 
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          addi      r5,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t74(r0),r5          
          lw        r5,t74(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r6,r0,r5            
          sw        t75(r0),r6          
          lw        r5,t75(r0)          % AssignStat [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ]  ] 
          sw        t1(r0),r5           
          addi      r5,r0,145           % IntegerLiteral (145), Variable 145: integer
          sw        t76(r0),r5          
          lw        r5,t76(r0)          % AssignStat [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  IntegerLiteral (145), Variable 145: integer ] 
          sw        t3(r0),r5           
          addi      r5,r0,7             % IntegerLiteral (7), Variable 7: integer
          sw        t77(r0),r5          
          lw        r5,t77(r0)          % AssignStat [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  IntegerLiteral (7), Variable 7: integer ] 
          sw        t5(r0),r5           
          addi      r5,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t78(r0),r5          
          lw        r5,t78(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r6,r0,r5            
          sw        t79(r0),r6          
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  EqualEqual (==) Minus (-), Variable -: integer [ IntegerLiteral (3), Varia
          lw        r6,t79(r0)          
          ceq       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  EqualEqual (==) Minus (-), Variable -: integer [ IntegerLitera
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  EqualEqual (==) VarElementList, Variable x: integer [ DataMember, Variable
          lw        r6,t1(r0)           
          ceq       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  EqualEqual (==) VarElementList, Variable x: integer [ DataMemb
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  EqualEqual (==) VarElementList, Variable y: integer [ DataMember, Variable
          lw        r6,t3(r0)           
          ceq       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  EqualEqual (==) VarElementList, Variable y: integer [ DataMemb
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          addi      r5,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t81(r0),r5          
          lw        r5,t81(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r6,r0,r5            
          sw        t82(r0),r6          
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtGt (<>) Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3:
          lw        r6,t82(r0)          
          cne       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtGt (<>) Minus (-), Variable -: integer [ IntegerLiteral (3),
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtGt (<>) VarElementList, Variable x: integer [ DataMember, Variable x: in
          lw        r6,t1(r0)           
          cne       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtGt (<>) VarElementList, Variable x: integer [ DataMember, Va
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtGt (<>) VarElementList, Variable y: integer [ DataMember, Variable y: in
          lw        r6,t3(r0)           
          cne       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtGt (<>) VarElementList, Variable y: integer [ DataMember, Va
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          addi      r5,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t83(r0),r5          
          lw        r5,t83(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r6,r0,r5            
          sw        t84(r0),r6          
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Lt (<) Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: in
          lw        r6,t84(r0)          
          clt       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Lt (<) Minus (-), Variable -: integer [ IntegerLiteral (3), Va
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Lt (<) VarElementList, Variable x: integer [ DataMember, Variable x: integ
          lw        r6,t1(r0)           
          clt       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Lt (<) VarElementList, Variable x: integer [ DataMember, Varia
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Lt (<) VarElementList, Variable y: integer [ DataMember, Variable y: integ
          lw        r6,t3(r0)           
          clt       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Lt (<) VarElementList, Variable y: integer [ DataMember, Varia
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          addi      r5,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t85(r0),r5          
          lw        r5,t85(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r6,r0,r5            
          sw        t86(r0),r6          
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtEqual (<=) Minus (-), Variable -: integer [ IntegerLiteral (3), Variable
          lw        r6,t86(r0)          
          cle       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtEqual (<=) Minus (-), Variable -: integer [ IntegerLiteral (
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtEqual (<=) VarElementList, Variable x: integer [ DataMember, Variable x:
          lw        r6,t1(r0)           
          cle       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtEqual (<=) VarElementList, Variable x: integer [ DataMember,
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtEqual (<=) VarElementList, Variable y: integer [ DataMember, Variable y:
          lw        r6,t3(r0)           
          cle       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  LtEqual (<=) VarElementList, Variable y: integer [ DataMember,
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          addi      r5,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t87(r0),r5          
          lw        r5,t87(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r6,r0,r5            
          sw        t88(r0),r6          
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Gt (>) Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: in
          lw        r6,t88(r0)          
          cgt       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Gt (>) Minus (-), Variable -: integer [ IntegerLiteral (3), Va
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Gt (>) VarElementList, Variable x: integer [ DataMember, Variable x: integ
          lw        r6,t1(r0)           
          cgt       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Gt (>) VarElementList, Variable x: integer [ DataMember, Varia
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Gt (>) VarElementList, Variable y: integer [ DataMember, Variable y: integ
          lw        r6,t3(r0)           
          cgt       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Gt (>) VarElementList, Variable y: integer [ DataMember, Varia
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          addi      r5,r0,3             % IntegerLiteral (3), Variable 3: integer
          sw        t89(r0),r5          
          lw        r5,t89(r0)          % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
          sub       r6,r0,r5            
          sw        t90(r0),r6          
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  GtEqual (>=) Minus (-), Variable -: integer [ IntegerLiteral (3), Variable
          lw        r6,t90(r0)          
          cge       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  GtEqual (>=) Minus (-), Variable -: integer [ IntegerLiteral (
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  GtEqual (>=) VarElementList, Variable x: integer [ DataMember, Variable x:
          lw        r6,t1(r0)           
          cge       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  GtEqual (>=) VarElementList, Variable x: integer [ DataMember,
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  GtEqual (>=) VarElementList, Variable y: integer [ DataMember, Variable y:
          lw        r6,t3(r0)           
          cge       r7,r5,r6            
          sw        t80(r0),r7          
          lw        r5,t80(r0)          % WriteStat [ RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  GtEqual (>=) VarElementList, Variable y: integer [ DataMember,
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
          lw        r6,t3(r0)           
          bz        r5,l57              
          bz        r6,l57              
          addi      r7,r0,1             
          j         l58                 
l57       addi      r7,r0,0             
l58       sw        t91(r0),r7          
          lw        r5,t3(r0)           % Minus (-), Variable -: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ 
          lw        r6,t1(r0)           
          sub       r7,r6,r5            
          sw        t92(r0),r7          
          lw        r5,t91(r0)          % RelExpr, Variable and: integer [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [
          lw        r6,t92(r0)          
          cge       r7,r5,r6            
          sw        t93(r0),r7          
          lw        r5,t93(r0)          % WriteStat [ RelExpr, Variable and: integer [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable 
          sw        -8(r14),r5          
          addi      r5,r0,buf           
          sw        -12(r14),r5         
          jl        r15,intstr          
          sw        -8(r14),r13         
          jl        r15,putstr          
          lb        r5,space(r0)        
          putc      r5                  
          lw        r5,t1(r0)           % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
          lw        r6,t3(r0)           
          bz        r5,l59              
          bz        r6,l59              
          addi      r7,r0,1             
          j         l60                 
l59       addi      r7,r0,0             
l60       sw        t94(r0),r7          
          lw        r5,t3(r0)           % Minus (-), Variable -: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ 
          lw        r6,t1(r0)           
          sub       r7,r6,r5            
          sw        t95(r0),r7          
          lw        r5,t94(r0)          % RelExpr, Variable and: integer [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [
          lw        r6,t95(r0)          
          cge       r7,r5,r6            
          sw        t96(r0),r7          
          lw        r5,t96(r0)          % AssignStat [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  RelExpr, Variable and: integer [ And (and), Variable and: integer [ VarElementList, Variable
          sw        t1(r0),r5           
          hlt                           

t1        res       4    % VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ] 
t2        res       4    % IntegerLiteral (35), Variable 35: integer
t3        res       4    % VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ] 
t4        res       4    % IntegerLiteral (3), Variable 3: integer
t5        res       4    % VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ] 
t6        res       4    % IntegerLiteral (2), Variable 2: integer
t7        res       4    % VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ] 
t8        res       4    % IntegerLiteral (4), Variable 4: integer
t9        res       4    % VarElementList, Variable b: integer [ DataMember, Variable b: integer [ Id (b) IndexList ]  ] 
t10       res       4    % IntegerLiteral (5), Variable 5: integer
t11       res       4    % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ I
t12       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable z: integer [ DataMember, Variable z: integer [ 
t13       res       4    % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
t14       res       4    % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer [ DataMember, Variable a: integer [ 
t15       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ D
t16       res       4    % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
t17       res       4    % Slash (/), Variable /: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable b: integer [ DataMember, Variable b: integer [ 
t18       res       4    % Asterisk (*), Variable *: integer [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  VarElementList, Variable b: integer [ DataMember, Variable b: integer
t19       res       4    % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  Asterisk (*), Variable *: integer [ VarElementList, Variable a: integer 
t20       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ D
t21       res       4    % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
t22       res       4    % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer [ DataMember, Variable a: integer [ 
t23       res       4    % Slash (/), Variable /: integer [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  VarElementList, Variable z: integer [ DataMember, Variable z: integer [ 
t24       res       4    % Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer [ DataMember, Variable a: integer [ 
t25       res       4    % Asterisk (*), Variable *: integer [ Slash (/), Variable /: integer [ VarElementList, Variable z: integer [ DataMember, Variable z: integer [ Id (z) IndexList ]  ]  VarElementList, Variable a: integer 
t26       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  Asterisk (*), Variable *: integer [ Slash (/), Variable /: integer [ Var
t27       res       4    % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ Da
t28       res       4    % IntegerLiteral (49), Variable 49: integer
t29       res       4    % Asterisk (*), Variable *: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  IntegerLiteral (49), Variable 49: integer ] 
t30       res       4    % Minus (-), Variable -: integer [ Asterisk (*), Variable *: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  IntegerLiteral (49), Variable 49: in
t31       res       4    % Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ Asterisk (*), Variable *: integer [ VarE
t32       res       4    % Asterisk (*), Variable *: integer [ VarElementList, Variable a: integer [ DataMember, Variable a: integer [ Id (a) IndexList ]  ]  VarElementList, Variable b: integer [ DataMember, Variable b: integer
t33       res       4    % Slash (/), Variable /: integer [ Plus (+), Variable +: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  Minus (-), Variable -: integer [ Asteris
t34       res       4    % VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ] 
t35       res       4    % IntegerLiteral (0), Variable 0: integer
t36       res       4    % VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ] 
t37       res       4    % IntegerLiteral (1), Variable 1: integer
t38       res       4    % IntegerLiteral (3), Variable 3: integer
t39       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t40       res       4    % IntegerLiteral (145), Variable 145: integer
t41       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ] 
t42       res       4    % And (and), Variable and: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable one
t43       res       4    % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable 
t44       res       4    % And (and), Variable and: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable ze
t45       res       4    % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable
t46       res       4    % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
t47       res       4    % And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer 
t48       res       4    % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer 
t49       res       4    % And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
t50       res       4    % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y:
t51       res       4    % And (and), Variable and: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: in
t52       res       4    % And (and), Variable and: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x:
t53       res       4    % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: in
t54       res       4    % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable one
t55       res       4    % Or (or), Variable or: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero:
t56       res       4    % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable ze
t57       res       4    % Or (or), Variable or: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  VarElementList, Variable one: integer [ DataMember, Variable one: i
t58       res       4    % Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ I
t59       res       4    % Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer [ I
t60       res       4    % Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: integer [ I
t61       res       4    % Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ I
t62       res       4    % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: in
t63       res       4    % Or (or), Variable or: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: integ
t64       res       4    % Or (or), Variable or: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  VarElementList, Variable x: integer [ DataMember, Variable x: in
t65       res       4    % Or (or), Variable or: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable zero: integer [ DataMember, Variable zero: integ
t66       res       4    % Not (not), Variable not: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  ] 
t67       res       4    % Not (not), Variable not: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  ] 
t68       res       4    % Not (not), Variable not: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  ] 
t69       res       4    % Not (not), Variable not: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ] 
t70       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable zero: integer [ DataMember, Variable zero: integer [ Id (zero) IndexList ]  ]  ] 
t71       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable one: integer [ DataMember, Variable one: integer [ Id (one) IndexList ]  ]  ] 
t72       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  ] 
t73       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable y: integer [ DataMember, Variable y: integer [ Id (y) IndexList ]  ]  ] 
t74       res       4    % IntegerLiteral (3), Variable 3: integer
t75       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t76       res       4    % IntegerLiteral (145), Variable 145: integer
t77       res       4    % IntegerLiteral (7), Variable 7: integer
t78       res       4    % IntegerLiteral (3), Variable 3: integer
t79       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t80       res       4    % RelExpr, Variable x: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  EqualEqual (==) Minus (-), Variable -: integer [ IntegerLiteral (3), Varia
t81       res       4    % IntegerLiteral (3), Variable 3: integer
t82       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t83       res       4    % IntegerLiteral (3), Variable 3: integer
t84       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t85       res       4    % IntegerLiteral (3), Variable 3: integer
t86       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t87       res       4    % IntegerLiteral (3), Variable 3: integer
t88       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t89       res       4    % IntegerLiteral (3), Variable 3: integer
t90       res       4    % Minus (-), Variable -: integer [ IntegerLiteral (3), Variable 3: integer ] 
t91       res       4    % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
t92       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ 
t93       res       4    % RelExpr, Variable and: integer [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [
t94       res       4    % And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer 
t95       res       4    % Minus (-), Variable -: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [ DataMember, Variable y: integer [ 
t96       res       4    % RelExpr, Variable and: integer [ And (and), Variable and: integer [ VarElementList, Variable x: integer [ DataMember, Variable x: integer [ Id (x) IndexList ]  ]  VarElementList, Variable y: integer [
buf       res       20   % Buffer space used for console output
space     db        " "  % Separator for console output