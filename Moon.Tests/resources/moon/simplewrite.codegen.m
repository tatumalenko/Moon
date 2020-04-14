          entry                         
          addi      r14,r0,topaddr      
          addi      r2,r0,1             
          sw        t1(r0),r2           
          lw        r1,t1(r0)           % processing: write(t1)
          sw        -8(r14),r1          % put value on stack
          addi      r1,r0,buf           % link buffer to stack
          sw        -12(r14),r1         
          jl        r15,intstr          % convert int to string for output
          sw        -8(r14),r13         
          jl        r15,putstr          % output to console
          hlt                           

t1        res       4    
buf       res       20   % buffer space used for console output
