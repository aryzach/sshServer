module TestLib where

import Text.ParserCombinators.Parsec --(try, noneOf, sepBy, oneOf, char, digit, satisfy, many1, choice, chainl1, parse, many, (<|>), Parser)

c :: Parser Char
c = char '\ETX'

p' :: String -> Either ParseError Char
p' input = parse c "(unknown)" input


