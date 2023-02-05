// writing constant command
// writing push stack 7
@7
D=A
@SP
A=M
M=D
@SP
M=M+1
// writing constant command
// writing push stack 8
@8
D=A
@SP
A=M
M=D
@SP
M=M+1
// writing plus command
// writing pop stack R13
@SP
M=M-1
@SP
A=M
D=M
@R13
M=D
// writing pop stack R14
@SP
M=M-1
@SP
A=M
D=M
@R14
M=D
// writing binary command +
@R13
D=M
@R14
M=D+M
// writing push stack R14
@R14
D=A
@SP
A=M
M=D
@SP
M=M+1
