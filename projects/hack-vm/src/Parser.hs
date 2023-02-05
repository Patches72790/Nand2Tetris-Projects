module Parser
  ( parseOpCodes,
    OpCode (..),
    Command (..),
    UnaryCommand (..),
    ControlFlowCommand (..),
    BinaryCommand (..),
    MemorySegment (..),
  )
where

data OpCode
  = Expression Command
  | Push MemorySegment Int
  | Pop MemorySegment Int
  | Label String
  | GoTo String
  | If String
  | Function String Int
  | Return
  | Call String Int
  deriving (Show)

data Command
  = Binary BinaryCommand
  | Unary UnaryCommand
  | ControlFlow ControlFlowCommand
  deriving (Show)

data BinaryCommand
  = Plus
  | Minus
  | And
  | Or
  deriving (Show)

data UnaryCommand
  = Neg
  | Not
  deriving (Show)

data ControlFlowCommand
  = Lt
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
parse ["add"] = Just (Expression (Binary Plus))
parse ["sub"] = Just (Expression (Binary Minus))
parse ["neg"] = Just (Expression (Unary Neg))
parse ["and"] = Just (Expression (Binary And))
parse ["or"] = Just (Expression (Binary Or))
parse ["not"] = Just (Expression (Unary Not))
parse ["eq"] = Just (Expression (ControlFlow Eq))
parse ["lt"] = Just (Expression (ControlFlow Lt))
parse ["gt"] = Just (Expression (ControlFlow Gt))
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
