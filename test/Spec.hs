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
 

test_parse' = test parse'
test_p' = test p'


main :: IO ()
main = do
  test_parse' "ls\r" Ls
  test_parse' "  ls\r" Ls
  test_parse' "ls    \r" Ls
  test_parse' "  ls    \r" Ls
  test_parse' "ls\ETX" Esc
  test_parse' "  ls   \ETX" Esc
  test_parse' "ls    \ETX" Esc
  test_parse' "2\ETX" $ Esc 
  test_parse' "2 \ETX" $ Esc 
  --test_parse' "\r"    $ C [] 
  test_p' "\ETX" '\ETX'


