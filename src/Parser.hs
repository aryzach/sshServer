module Parser where

import Text.Parsec hiding (try)
import Text.Parsec.String
import Text.ParserCombinators.Parsec 
import Control.Monad (void)


data Command  = Cat String
              | Esc
              | Ls
              | InvalidCommand
              deriving (Eq,Show)

full :: Parser Command
full = userCommands <|> try invalid <|> try escape 

line :: Parser Command -> Parser Command
line p = do
  whitespace
  c <- p
  whitespace
  eol
  return c

userCommands :: Parser Command
userCommands = choice $ map (try . line) [ls, cat]

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

invalid :: Parser Command
invalid = do
  --x <- many $ noneOf ['\ETX']
  --char '\r'
  manyTill (noneOf ['\ETX']) $ char '\r'
  return $ InvalidCommand

fileName :: Parser String
fileName = do
  name <- many alphaNum
  dot  <- char '.'
  md   <- string "md"
  return $ name ++ ".md"

eol :: Parser ()
eol = void $ char '\r'

escape :: Parser Command
escape = do
  manyTill anyChar $ char '\ETX'
  --char '\ETX'
  --endBy (many anyChar) $ char '\ETX'
  return Esc

parse' :: String -> Either ParseError Command
parse' input = parse full "(unknown)" input

pt :: Parser a -> String -> Either ParseError a
pt p input = parse p "(unknown)" input

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"



