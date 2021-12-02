load DecideJump.hdl,
output-file DecideJump.out,
compare-to DecideJump.cmp,
output-list shouldJump%D6.1.5;

set j1 %B0, // J = 000
set j2 %B0,
set j3 %B0,
set zr %B0,
set ng %B0,
eval,
output;

set j3 %B1, // J = 001
set zr %B0,
set ng %B0,
eval,
output;

set zr %B1,  // J = 001 zr=1 ng=0
set ng %B0,
eval, output;

set zr %B0, // J = 001 zr=0 ng=1
set ng %B1,
eval, output;

set zr %B1, // J = 001 zr = 1 ng=1
set ng %B1,
eval, output;

set j2 %B1, set j1 %B1, set j3 %B1, // J = 111 zr = 0 ng =0 
set zr %B0, set ng %B0,
eval, output;

set zr %B1,  // J = 111 zr=1 ng=0
set ng %B0,
eval, output;

set zr %B0, // J = 111 zr=0 ng=1
set ng %B1,
eval, output;

set zr %B1, // J = 111 zr = 1 ng=1
set ng %B1,
eval, output;

