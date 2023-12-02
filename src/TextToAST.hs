{-
-- EPITECH PROJECT, 2023
-- Text To AST
-- File description:
-- TextToAST
-}

module TextToAST
    (
        textToAST
    ) where

import AST
import Data.Int (Int64)
import Data.Char (isDigit)

isFunction :: String -> Bool
isFunction [] = False
isFunction ('(':_) = True
isFunction _ = False

skipableChar :: Char -> Bool
skipableChar x = x == ' ' || x == '\t'

notSkipableChar :: Char -> Bool
notSkipableChar x = not (skipableChar x)

stringIsNumber :: String -> Bool
stringIsNumber [] = False
stringIsNumber (x:[]) | Data.Char.isDigit x = True
                      | otherwise = False
stringIsNumber (x:xs) | Data.Char.isDigit x = stringIsNumber xs
                      | otherwise = False

nextToParse :: String -> String
nextToParse [] = []
nextToParse (x:xs) | skipableChar x = nextToParse xs
nextToParse str = dropWhile notSkipableChar str

createNodeFromFunction :: Symbol -> String -> Maybe Tree
createNodeFromFunction [] _ = Nothing
createNodeFromFunction _ [] = Nothing
createNodeFromFunction (_:xs) str = Just (Node xs (textToAST str)
                      (textToAST (nextToParse str)))

stringIsBool :: String -> Bool
stringIsBool str = str == "#t" || str == "#f"

createBool :: String -> Maybe Tree
createBool "#t" = Just (Leaf (Boolean True))
createBool "#f" = Just (Leaf (Boolean False))
createBool _ = Nothing

treeFromAtom :: String -> String -> Maybe Tree
treeFromAtom [] _ = Nothing
treeFromAtom split _ | stringIsNumber split =
                        Just (Leaf (AST.Number (read split :: Data.Int.Int64)))
                     | stringIsBool split = createBool split
treeFromAtom split str | isFunction split =
                          createNodeFromFunction split (init str)
                       | otherwise = Just (Leaf (Symbol split))

textToAST :: String -> Maybe Tree
textToAST [] = Nothing
textToAST (x:xs) | skipableChar x = textToAST xs
textToAST str = treeFromAtom (takeWhile notSkipableChar str)
                  (dropWhile notSkipableChar str)
