module CodeWriter (writeCode) where

import Parser (OpCode (..))

writeALiteral :: Int -> String
writeALiteral i = "@" ++ show i

writeAVariable :: String -> String
writeAVariable s = "@" ++ s

writeCAssign :: String -> String -> String
writeCAssign lhs rhs = lhs ++ "=" ++ rhs

writeCode :: [OpCode] -> Bool
writeCode _ = False
