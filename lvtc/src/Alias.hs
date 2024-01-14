{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Alias
-}

module Alias (
    proceedAlias,
) where

import Expression
import Parser
import ParseUtil
import Control.Applicative

data Alias = Alias String String

instance Show Alias.Alias where
    show (Alias.Alias str1 str2) = "ALIAS `" ++ str1 ++ "`:`" ++ str2 ++ "`"

parseAliasKeyword :: Parser String
parseAliasKeyword = parseString "alias "

parseAliasName :: Parser String
parseAliasName = parseAllCharUntil " "

parseAliasValue :: Parser String
parseAliasValue = parseAllCharUntil ";\n"

parseAlias' :: Parser String
parseAlias' = (parseAliasKeyword *> parseAliasName <* many (parseChar ' '))

parseAlias :: Parser Alias.Alias
parseAlias = Parser f
    where
        f str = case runParser parseAlias' str of
            Nothing -> Nothing
            Just (key, xs) -> case runParser parseAliasValue xs of
                Nothing -> Nothing
                Just (value, ys) -> Just (Alias.Alias key value, ys)

replaceAliasInString :: Alias.Alias -> String -> String
replaceAliasInString _ [] = []
replaceAliasInString (Alias.Alias key value) (x:xs)
    | take (length key) (x:xs) == key =
        value ++ replaceAliasInString
                    (Alias.Alias key value)
                    (drop (length key) (x:xs))
    | otherwise = x : replaceAliasInString (Alias.Alias key value) xs

replaceAlias :: Alias -> [Expression] -> [Expression]
replaceAlias _ [] = []
replaceAlias alias ((Expression.Alias _):xs) =
    replaceAlias alias xs
replaceAlias (Alias.Alias key value) ((Expression.Function str):xs) =
    (Expression.Function (replaceAliasInString (Alias.Alias key value) str))
    : (replaceAlias (Alias.Alias key value) xs)
replaceAlias (Alias.Alias key value) ((Expression.Comment str):xs) =
    (Expression.Comment (replaceAliasInString (Alias.Alias key value) str))
    : (replaceAlias (Alias.Alias key value) xs)

replaceAllAlias :: [Alias] -> [Expression] -> [Expression]
replaceAllAlias [] exprs = exprs
replaceAllAlias _ [] = []
replaceAllAlias (x:xs) exprs = replaceAllAlias xs newExprs
    where
        newExprs = replaceAlias x exprs

getListAlias :: [Expression] -> [Alias]
getListAlias [] = []
getListAlias ((Expression.Alias str):xs) = case runParser parseAlias str of
    Just (alias, _) -> alias : getListAlias xs
    Nothing -> getListAlias xs
getListAlias (_:xs) = getListAlias xs

proceedAlias :: [Expression] -> [Expression]
proceedAlias exprs = replaceAllAlias lstAlias exprs
    where
        lstAlias = getListAlias exprs
