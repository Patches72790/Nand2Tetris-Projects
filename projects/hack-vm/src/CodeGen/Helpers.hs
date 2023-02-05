module CodeGen.Helpers
  ( writeALiteral,
    writeAVariable,
    writeCAssign,
    popStack,
    pushConstant,
    writeBinary,
    writeControlFlow,
    writeUnary,
  )
where

import Parser (ControlFlowCommand)

writeALiteral :: Int -> String
writeALiteral i = "@" ++ show i

writeAVariable :: String -> String
writeAVariable s = "@" ++ s

writeCAssign :: String -> String -> String
writeCAssign lhs rhs = lhs ++ "=" ++ rhs

{--
-- read from reg r13
-- read from reg r14
-- perform binary cmd on contents
-- store result back on stack
--}
writeBinary :: String -> [String]
writeBinary cmd =
  concat
    [ popStack "R13",
      popStack "R14",
      [ "// writing binary command " ++ cmd,
        writeAVariable "R13",
        writeCAssign "D" "M",
        writeAVariable "R14",
        writeCAssign "M" "D" ++ cmd ++ "M"
      ],
      pushStack "R14"
    ]

writeControlFlow :: ControlFlowCommand -> [String]
writeControlFlow cmd = []

writeUnary :: String -> [String]
writeUnary cmd = []

popStack :: String -> [String]
popStack reg =
  [ "// writing pop stack " ++ reg,
    writeAVariable "SP",
    writeCAssign "M" "M-1",
    writeAVariable "SP",
    writeCAssign "A" "M",
    writeCAssign "D" "M",
    writeAVariable reg,
    writeCAssign "M" "D"
  ]

pushStack :: String -> [String]
pushStack num =
  [ "// writing push stack " ++ num,
    writeAVariable num,
    writeCAssign "D" "A",
    writeAVariable "SP",
    writeCAssign "A" "M",
    writeCAssign "M" "D",
    writeAVariable "SP",
    writeCAssign "M" "M+1"
  ]

pushConstant :: Int -> [String]
pushConstant i = pushStack $ show i
