module Parse.Lit where

import Parse.Util
import Text.Megaparsec

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L hiding (lexeme)

--A string literal can contain any character inside quotes
litString :: Parser String
litString = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

litChar :: Parser Char
litChar = lexeme $ between (char '\'') (char '\'') L.charLiteral

litInt :: Parser Int
litInt = lexeme $ decimal

litDouble :: Parser Double
litDouble = lexeme $ float 