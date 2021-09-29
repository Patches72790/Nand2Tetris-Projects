load ALU.hdl,
compare-to myALU.cmp,
output-file myALUtest.out,
output-list out%B1.16.1 zr%B1.1.1 ng%B1.1.1;

set x %B0000000000000000,  // x = 0
set y %B1111111111111111,  // y = -1

set f 1, // x + y
eval,
output;

set f 0, // x & y
eval,
output;

set no 1, // !(x & y)
eval,
output;


