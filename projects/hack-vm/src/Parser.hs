module Parser
(
    parseOpCodes
) where

import Control.Applicative

data OpCode = 
    Arithmetic BinaryCommand |
    Push MemorySegment Int |
    Pop MemorySegment Int |
    Label String |
    GoTo String |
    If String |
    Function String Int |
    Return |
    Call String Int
    deriving (Show)

data BinaryCommand = 
    Plus |
    Minus |
    And |
    Neg |
    Or |
    Not |
    Lt |
    Gt |
    Eq
    deriving (Show)


data MemorySegment = 
    Argument |
    Local |
    This | 
    That|
    Constant |
    Static |
    Temp|
    Pointer
    deriving (Show)

parseOpCodes :: [[String]] -> Maybe [OpCode]
parseOpCodes [] = Nothing
parseOpCodes (line: rest) = liftA2 (++) (mapM parse line) (parseOpCodes rest)

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
parse (token:rest)
    | token == "push" 
        || token == "pop" = parseStackCommand (token:rest)
    | token == "label"
        || token == "goto"
        || token == "if-goto" = parseBranchCommand (token:rest)
    | token == "function"
        || token == "call"
        || token == "return" = parseFunctionCommand (token:rest)
parse _ = Nothing

-- Create push/pop command with various memory segment + offset ints
parseStackCommand :: [String] -> Maybe OpCode
parseStackCommand _ = Nothing

-- todo
parseBranchCommand :: [String] -> Maybe OpCode
parseBranchCommand _ = Nothing

-- todo
parseFunctionCommand :: [String] -> Maybe OpCode
parseFunctionCommand _ = Nothing
