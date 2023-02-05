module Lib
  ( someFunc,
    wordsOfLines,
    writeCodeToFile,
  )
where

import CodeWriter (writeCode)
import Parser (parseOpCodes)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

wordsOfLines :: String -> [[String]]
wordsOfLines text = map words (lines text)

writeCodeToFile :: Maybe [String] -> Handle -> IO ()
writeCodeToFile Nothing _ = print "Cannot write no code to file"
writeCodeToFile (Just code) handle = hPutStr handle (unlines code)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  case args of
    [] -> do
      print "Usage: ./vmtranslator [filename]"
      exitFailure
    (filename : _) -> do
      print filename

      contents <- readFile filename
      print contents

      let code = writeCode $ parseOpCodes $ wordsOfLines contents
      print code

      let outFile = takeWhile (/= '.') filename
      print outFile

-- todo wriTe to file
