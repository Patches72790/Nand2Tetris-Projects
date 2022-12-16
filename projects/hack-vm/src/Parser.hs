module Parser
(
    parse
) where

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


data MemorySegment = 
    Argument |
    Local |
    This | 
    That|
    Constant |
    Static |
    Temp|
    Pointer

parse :: String -> Maybe [OpCode]
parse _ = Nothing
