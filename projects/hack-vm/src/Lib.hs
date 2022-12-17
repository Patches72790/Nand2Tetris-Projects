module Lib
    ( someFunc
    ) where

import Parser (parseOpCodes)
import System.IO

wordsOfLines :: String -> [[String]]
wordsOfLines text = map words (lines text)

someFunc :: IO ()
someFunc = do
    handle <- openFile "test_files/short_test.vm" ReadMode
    contents <- hGetContents handle
    let allTheWords = wordsOfLines contents
    print allTheWords
    let tokens = parseOpCodes allTheWords
    print tokens
    hClose handle
