// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
(CHECK)
@24576
D=M
@PRE_LOAD
D;JNE

// sets the beginning of screen ptr var
@SCREEN
D=A
@scr_ptr
M=D

// clears the screen
(CLEAR)
@scr_ptr
A=M
M=0
@scr_ptr
A=M
A=A+1
D=A
@scr_ptr
M=D
@24576
D=A-D
@CLEAR
D;JGT
@CHECK
0;JMP

// sets the beginning of screen ptr var
(PRE_LOAD)
@SCREEN
D=A
@scr_ptr
M=D

(LOAD)
@scr_ptr
A=M
M=-1
@scr_ptr
A=M
A=A+1
D=A
@scr_ptr
M=D
@24576
D=A-D
@LOAD
D;JGT
@CHECK
0;JMP
