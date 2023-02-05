module Lib
  ( someFunc,
    wordsOfLines,
    writeCodeToFile,
  )
where

import CodeWriter (writeCode)
import Parser (parseOpCodes)
import System.IO

wordsOfLines :: String -> [[String]]
wordsOfLines text = map words (lines text)

writeCodeToFile :: Maybe [String] -> Handle -> IO ()
writeCodeToFile Nothing _ = print "Cannot write no code to file"
writeCodeToFile (Just code) handle = hPutStr handle (unlines code)

someFunc :: IO ()
someFunc = do
  handle <- openFile "test_files/simpleadd.vm" ReadMode
  contents <- hGetContents handle
  let allTheWords = wordsOfLines contents
  print allTheWords
  let tokens = parseOpCodes allTheWords
  print "Tokens: "
  print tokens
  let code = writeCode tokens

  print "Code generated:"
  print code

  outFileHandle <- openFile "test_files/simpleadd.asm" WriteMode
  writeCodeToFile code outFileHandle

  hClose handle
  hClose outFileHandle
