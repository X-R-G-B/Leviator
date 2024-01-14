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

countBracketsForFunction :: Int -> String -> Int
countBracketsForFunction _ [] = 0
countBracketsForFunction 1 ['\n', '}', ';', '\n'] = 0
countBracketsForFunction n ['\n', '}', ';', '\n'] = n
countBracketsForFunction 1 ('}':_) = 1
countBracketsForFunction n ('{':xs) = countBracketsForFunction (n + 1) xs
countBracketsForFunction n ('}':xs) = countBracketsForFunction (n - 1) xs
countBracketsForFunction n ('\\':_:xs) = countBracketsForFunction n xs
countBracketsForFunction n (_:xs) = countBracketsForFunction n xs

parseFunction' :: Parser Expression
parseFunction' =
    (\x -> Function (x ++ "\n};\n"))
        <$> ((++)
            <$> (parseString "fn " <|> parseString "export fn")
                <*> parseAllCharUntil "\n};\n")

parseFunction :: Parser Expression
parseFunction = Parser f
    where
        f str = case runParser parseFunction' str of
            Nothing -> Nothing
            Just (Function x, xs) -> case countBracketsForFunction 0 x of
                0 -> Just (Function x, xs)
                _ -> Nothing
            Just _ -> Nothing

parseAlias :: Parser Expression
parseAlias =
    (\x -> Alias (x ++ ";\n"))
        <$> ((++)
            <$> parseString "alias "
                <*> parseAllCharUntil ";\n")

parseComment :: Parser Expression
parseComment =
    (\x -> Comment (x ++ "\n"))
        <$> ((++)
            <$> parseString "//"
                <*> parseAllCharUntil "\n")

parseExpresion :: Parser Expression
parseExpresion = parseAlias <|> parseFunction <|> parseComment

parseAllExpression :: Parser [Expression]
parseAllExpression = Parser f
    where
        p = parseExpresion <* many (parseAnyChar "\n")
        f [] = Just ([], [])
        f str = case runParser p str of
            Nothing -> Nothing
            Just (x, xs) -> case runParser parseAllExpression xs of
                Nothing -> Nothing
                Just (y, ys) -> Just (x : y, ys)
