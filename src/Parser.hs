module Parser where

import Text.Parsec hiding (try)
import Text.Parsec.String
import Text.ParserCombinators.Parsec 
import Control.Monad (void)
import qualified Data.Char as C


data Command  = Cat String
              | Esc
              | Ls
              | InvalidCommand
              | InvalidKey C.Char
              deriving (Eq,Show)

isValidKey :: C.Char -> Bool
isValidKey k = any (==True) $ map (\f -> f k) [C.isAlphaNum, C.isSpace, (=='\ETX'), (=='\r'), (=='.'), (=='\DEL')]

full :: Parser Command
full = userCommands <|> try invalidCommand <|> try escape 

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

invalidCommand :: Parser Command
invalidCommand = do
  manyTill (noneOf ['\ETX']) $ char '\r'
  return $ InvalidCommand

invalidKey :: Parser Command
invalidKey = do
  g <- satisfy (\k -> not $ isValidKey k)
  return $ InvalidKey g

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
  return Esc

parse' :: String -> Either ParseError Command
parse' input = parse full "(unknown)" input

pt :: Parser a -> String -> Either ParseError a
pt p input = parse p "(unknown)" input

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"



