
load DecodeInstruction.hdl,
output-file DecodeInstruction.out,
compare-to DecodeInstruction.cmp,
output-list comp%B1.6.1 dest%B1.3.2 jump%B1.3.2 useM%B1.4.1 isCInstr%B1.8.1;

// testing proceeds...

set instruction %Xfdd8,
eval,
output;

set instruction %Xffff,
eval,
output;

set instruction %Xedcb,
eval,
output;

