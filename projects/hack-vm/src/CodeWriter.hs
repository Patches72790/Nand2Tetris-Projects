module CodeWriter (writeCode) where

import CodeGen.Helpers
  ( pushConstant,
    writeArithmetic,
    writeLogical,
  )
import Parser
  ( BinaryCommand (..),
    MemorySegment (..),
    OpCode (..),
  )

writeCode :: Maybe [OpCode] -> Maybe [String]
writeCode Nothing = Nothing
writeCode (Just code) = Just $ interpretOpCodes code

interpretOpCodes :: [OpCode] -> [String]
interpretOpCodes ((Arithmetic c) : rest) = writeBinaryCommand c ++ interpretOpCodes rest
interpretOpCodes ((Push mem i) : rest) = writePushCommand mem i ++ interpretOpCodes rest
interpretOpCodes _ = []

writeBinaryCommand :: BinaryCommand -> [String]
writeBinaryCommand Plus = "// writing plus command" : writeArithmetic "+"
writeBinaryCommand Minus = "// writing minus command" : writeArithmetic "-"
writeBinaryCommand And = "// writing and command" : writeArithmetic "&"
writeBinaryCommand Or = "// writing or command" : writeArithmetic "|"
writeBinaryCommand Lt = "// writing lt command" : writeLogical "<"
writeBinaryCommand Gt = "// writing gt command" : writeLogical ">"
writeBinaryCommand Eq = "// writing eq command" : writeLogical "="
writeBinaryCommand c = error "Unimplemented binary command" c

writePushCommand :: MemorySegment -> Int -> [String]
writePushCommand Constant i = "// writing constant command" : pushConstant i
writePushCommand c _ = error "Unimplemented push command" c
