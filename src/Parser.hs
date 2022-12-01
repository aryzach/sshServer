module Parser where

import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec as PC
import Control.Monad (void)

data Command  = C [String] 
              | Esc
              | Ls
              deriving (Eq,Show)

full :: Parser Command
full = try (line ls) <|> escape

line :: Parser Command -> Parser Command
line p = do
  c <- p
  eol 
  return c

ls :: Parser Command
ls = do
  whitespace 
  string "ls"
  whitespace
  return Ls

cell :: Parser String
cell = many $ noneOf " \n\t\r\ETX"

eol :: Parser ()
eol = void $ char '\r'

escape :: Parser Command
escape = do
  manyTill anyChar $ char '\ETX'
  return Esc

parse' :: String -> Either ParseError Command
parse' input = parse full "(unknown)" input

pt :: Parser a -> String -> Either ParseError a
pt p input = parse p "(unknown)" input

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
