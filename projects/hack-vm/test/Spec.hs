import CodeWriter
import Parser (parseOpCodes)
import System.IO
import System.Process (callCommand)
import Test.Hspec

writeCodeToFile :: Maybe [String] -> Handle -> IO ()
writeCodeToFile Nothing _ = print "Cannot write no code to file"
writeCodeToFile (Just code) handle = hPutStr handle (unlines code)

wordsOfLines :: String -> [[String]]
wordsOfLines text = map words (lines text)

testSimpleAdd :: IO ()
testSimpleAdd = do
  fileHandle <- openFile "test_files/simpleadd.vm" ReadMode
  contents <- hGetContents fileHandle
  let commands = wordsOfLines contents
  let tokens = parseOpCodes commands
  let code = writeCode tokens
  outFileHandle <- openFile "test_files/SimpleAdd.asm" WriteMode
  writeCodeToFile code outFileHandle
  hClose fileHandle
  hClose outFileHandle
  print code
  callCommand "cp test_files/SimpleAdd.asm ../07/StackArithmetic/SimpleAdd/SimpleAdd.asm"

main :: IO ()
main = do
  testSimpleAdd
  print "Tests done"

test1 :: IO ()
test1 = hspec $ do
  describe "Spec tester" $ do
    it "runs the test" $ do
      "abc" `shouldBe` ("abc" :: String)
