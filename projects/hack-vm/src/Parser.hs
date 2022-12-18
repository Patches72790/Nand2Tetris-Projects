module Parser
  ( parseOpCodes,
    OpCode (..),
  )
where

data OpCode
  = Arithmetic BinaryCommand
  | Push MemorySegment Int
  | Pop MemorySegment Int
  | Label String
  | GoTo String
  | If String
  | Function String Int
  | Return
  | Call String Int
  deriving (Show)

data BinaryCommand
  = Plus
  | Minus
  | And
  | Neg
  | Or
  | Not
  | Lt
  | Gt
  | Eq
  deriving (Show)

data MemorySegment
  = Argument
  | Local
  | This
  | That
  | Constant
  | Static
  | Temp
  | Pointer
  deriving (Show)

parseOpCodes :: [[String]] -> Maybe [OpCode]
parseOpCodes = mapM parse

parse :: [String] -> Maybe OpCode
parse ["add"] = Just (Arithmetic Plus)
parse ["sub"] = Just (Arithmetic Minus)
parse ["neg"] = Just (Arithmetic Neg)
parse ["and"] = Just (Arithmetic And)
parse ["or"] = Just (Arithmetic Or)
parse ["not"] = Just (Arithmetic Not)
parse ["eq"] = Just (Arithmetic Eq)
parse ["lt"] = Just (Arithmetic Lt)
parse ["gt"] = Just (Arithmetic Gt)
parse (token : rest)
  | token == "push"
      || token == "pop" =
      parseStackCommand (token : rest)
  | token == "label"
      || token == "goto"
      || token == "if-goto" =
      parseBranchCommand (token : rest)
  | token == "function"
      || token == "call"
      || token == "return" =
      parseFunctionCommand (token : rest)
parse _ = Nothing

-- Create push/pop command with various memory segment + offset ints
parseStackCommand :: [String] -> Maybe OpCode
parseStackCommand ["push", memorySegment, index]
  | memorySegment == "constant" = Just (Push Constant (read index :: Int))
  | memorySegment == "argument" = Just (Push Constant (read index :: Int))
  | memorySegment == "local" = Just (Push Constant (read index :: Int))
  | memorySegment == "static" = Just (Push Constant (read index :: Int))
  | memorySegment == "this" = Just (Push Constant (read index :: Int))
  | memorySegment == "that" = Just (Push Constant (read index :: Int))
  | memorySegment == "pointer" = Just (Push Constant (read index :: Int))
  | memorySegment == "temp" = Just (Push Constant (read index :: Int))
parseStackCommand ["pop", memorySegment, index]
  | memorySegment == "constant" = Just (Pop Constant (read index :: Int))
  | memorySegment == "argument" = Just (Pop Constant (read index :: Int))
  | memorySegment == "local" = Just (Pop Constant (read index :: Int))
  | memorySegment == "static" = Just (Pop Constant (read index :: Int))
  | memorySegment == "this" = Just (Pop Constant (read index :: Int))
  | memorySegment == "that" = Just (Pop Constant (read index :: Int))
  | memorySegment == "pointer" = Just (Pop Constant (read index :: Int))
  | memorySegment == "temp" = Just (Pop Constant (read index :: Int))
parseStackCommand _ = Nothing

-- todo
parseBranchCommand :: [String] -> Maybe OpCode
parseBranchCommand _ = Nothing

-- todo
parseFunctionCommand :: [String] -> Maybe OpCode
parseFunctionCommand _ = Nothing
