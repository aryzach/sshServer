import Parser
import TestLib
import Text.ParserCombinators.Parsec 

test :: (Eq a, Show a, Show b) => (b -> Either ParseError a) -> b -> a -> IO ()
test p s a = do
  let r = p s
  if r == Right a 
  then do
    putStrLn "OK" 
  else do
    putStrLn $ "FAIL: " 
    print s 
    print $ r
    seperator 

seperator = do
  putStrLn ""
  putStrLn "-------------------"
 



main :: IO ()
main = do
  test parse' "ls\r" Ls
  test parse' "  ls\r" Ls
  test parse' "ls    \r" Ls
  test parse' "  ls    \r" Ls
  test parse' "ls\ETX" Esc
  test parse' "  ls   \ETX" Esc
  test parse' "ls    \ETX" Esc
  test parse' "2\ETX" $ Esc 
  test parse' "2 \ETX" $ Esc 
  test parse' "cat  asdf.md  \ETX" $ Esc 
  test parse' "cat  asdf.md  \r" $ Cat "asdf.md"
  test parse' "cat  asdf\r" $ InvalidCommand -- this should fail

  --test_parse' "\r"    $ C [] 
  test (test_parser etxParser) "\ETX" '\ETX'
  test (test_parser escape) "  \ETX" Esc


--parseTest (oneOf "aeiou"  >> parserTrace "label") "atest"
