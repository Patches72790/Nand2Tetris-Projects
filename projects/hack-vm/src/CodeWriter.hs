module CodeWriter (writeCode) where

import CodeGen.Helpers
  ( pushConstant,
    writeBinary,
    writeControlFlow,
    writeUnary,
  )
import Parser
  ( BinaryCommand (..),
    Command (..),
    ControlFlowCommand (..),
    MemorySegment (..),
    OpCode (..),
    UnaryCommand (..),
  )

writeCode :: Maybe [OpCode] -> Maybe [String]
writeCode Nothing = Nothing
writeCode (Just code) = Just $ interpretOpCodes code

interpretOpCodes :: [OpCode] -> [String]
interpretOpCodes ((Expression c) : rest) = writeExpression c ++ interpretOpCodes rest
interpretOpCodes ((Push mem i) : rest) = writePushCommand mem i ++ interpretOpCodes rest
interpretOpCodes _ = []

writeExpression :: Command -> [String]
writeExpression (Binary c) = writeBinaryCommand c
writeExpression (Unary c) = writeUnaryCommand c
writeExpression (ControlFlow c) = writeControlFlowCommand c

writeBinaryCommand :: BinaryCommand -> [String]
writeBinaryCommand Plus = "// writing plus command" : writeBinary "+"
writeBinaryCommand Minus = "// writing minus command" : writeBinary "-"
writeBinaryCommand And = "// writing and command" : writeBinary "&"
writeBinaryCommand Or = "// writing or command" : writeBinary "|"

writeUnaryCommand :: UnaryCommand -> [String]
writeUnaryCommand Neg = writeUnary "-"
writeUnaryCommand Not = writeUnary "!"

writeControlFlowCommand :: ControlFlowCommand -> [String]
writeControlFlowCommand Lt = writeControlFlow Lt
writeControlFlowCommand Gt = writeControlFlow Gt
writeControlFlowCommand Eq = writeControlFlow Eq

writePushCommand :: MemorySegment -> Int -> [String]
writePushCommand Constant i = "// writing constant command" : pushConstant i
writePushCommand c _ = error "Unimplemented push command" c
