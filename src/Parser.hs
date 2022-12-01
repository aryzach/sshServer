module Parser where

import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec as PC
import Control.Monad (void)

data Command  = Cat String
              | Esc
              | Ls
              deriving (Eq,Show)

full :: Parser Command
full = try userCommands <|> escape

line :: Parser Command -> Parser Command
line p = do
  whitespace
  c <- p
  whitespace
  eol
  return c

userCommands :: Parser Command
userCommands = choice $ map line [ls, cat]

ls :: Parser Command
ls = do
  string "ls"
  return Ls

cat :: Parser Command
cat = do
  string "cat"
  char ' '
  whitespace
  x <- fileName
  return $ Cat x

fileName :: Parser String
fileName = do
  name <- many alphaNum
  dot  <- char '.'
  md   <- string "md"
  return name

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
