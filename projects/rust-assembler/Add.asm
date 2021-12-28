// Computes R0 ; 2 + 3  (R0 refers to RAM[0])

(PROGRAM)
@2
D;JGT
@3
A;JGE
@0
M;JMP
@hello
(END)
@goodbyte
AMD=M+1
