module TestLib where

import Text.ParserCombinators.Parsec --(try, noneOf, sepBy, oneOf, char, digit, satisfy, many1, choice, chainl1, parse, many, (<|>), Parser)

etxParser :: Parser Char
etxParser = char '\ETX'

test_parser :: Parser a -> String -> Either ParseError a
test_parser p input = parse p "(unknown)" input

