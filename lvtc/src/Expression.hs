{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Expression
-}

module Expression (
    Expression (..),
    parseExpresion,
    parseAllExpression,
) where

import Parser
import Control.Applicative
import ParseUtil

data Expression = Function String | Alias String | Comment String

instance Show Expression where
    show (Function str) = "F:`" ++ str ++ "`"
    show (Alias str) = "A:`" ++ str ++ "`"
    show (Comment str) = "C:`" ++ str ++ "`"

instance Eq Expression where
    (==) (Function str1) (Function str2) = str1 == str2
    (==) (Alias str1) (Alias str2) = str1 == str2
    (==) (Comment str1) (Comment str2) = str1 == str2
    (==) _ _ = False

parseFunction :: Parser Expression
parseFunction = Function <$> ((++) <$> (parseString "fn " <|> parseString "export fn") <*> parseAllCharUntil "\n};\n")

parseAlias :: Parser Expression
parseAlias = Alias <$> ((++) <$> parseString "alias " <*> parseAllCharUntil ";\n")

parseComment :: Parser Expression
parseComment = Comment <$> ((++) <$> parseString "//" <*> parseAllCharUntil "\n")

parseExpresion :: Parser Expression
parseExpresion = parseAlias <|> parseFunction <|> parseComment

parseAllExpression :: Parser [Expression]
parseAllExpression = some p
    where
        p = parseExpresion <* many (parseAnyChar "\n")
