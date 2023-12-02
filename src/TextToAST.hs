{-
--  EPITECH PROJECT, 2023
--  Text To AST
--  File description:
--  TextToAST
-}

module TextToAST
    (
        textToAST
    ) where

import AST
import Data.Int (Int64)
import Data.Char (isDigit)
import Debug.Trace

isFunction :: String -> Bool
isFunction [] = False
isFunction (x:_) | x == '(' = True
                 | otherwise = False

skipableChar :: Char -> Bool
skipableChar x = x == ' ' || x == '\t'

notSkipableChar :: Char -> Bool
notSkipableChar x = not (skipableChar x)

stringIsNumber :: String -> Bool
stringIsNumber [] = False
stringIsNumber (x:[]) | Data.Char.isDigit x = True
stringIsNumber (x:xs) | Data.Char.isDigit x = stringIsNumber xs
                      | otherwise = False

nextToParse :: String -> String
nextToParse [] = []
nextToParse (x:xs) | skipableChar x = nextToParse xs
nextToParse str = dropWhile notSkipableChar str

createNode :: Symbol -> String -> Maybe Tree
createNode [] _ = Nothing
createNode _ [] = Nothing
createNode (_:xs) str = Just (Node xs (textToAST str) (textToAST (nextToParse str)))

stringIsBool :: String -> Bool
stringIsBool str = str == "#t" || str == "#f"

createBool :: String -> Maybe Tree
createBool str | str == "#t" = Just (Leaf (Boolean True))
               | str == "#f" = Just (Leaf (Boolean False))
               | otherwise = Nothing

treeFromAtom :: String -> String -> Maybe Tree
treeFromAtom [] _ = Nothing
treeFromAtom split _ | stringIsNumber split = Just (Leaf (AST.Number (read split :: Data.Int.Int64)))
                     | stringIsBool split = createBool split
treeFromAtom split str | isFunction split = createNode split (init str)
                       | otherwise = Just (Leaf (Symbol split))

textToAST :: String -> Maybe Tree
textToAST [] = Nothing
textToAST (x:xs) | skipableChar x = textToAST xs
textToAST str = treeFromAtom (takeWhile notSkipableChar str) (dropWhile notSkipableChar str)
