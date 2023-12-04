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
skipableChar ' ' = True
skipableChar '\t' = True
skipableChar _ = False

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
nextToParse ('(':xs)
nextToParse str = dropWhile skipableChar (dropWhile notSkipableChar (dropWhile skipableChar (x:xs))

countAtoms :: String -> Int -> Int
countAtoms str depth | depth >= 2 = 2
                     | len str > 0 = countAtoms (nextToParse str) (depth + 1)
                     | otherwise = depth

createVariadic :: String -> Variadic
createVariadic str = Variadic (textToAST str) (textToAST (nextToParse str))

createNodeFromFunction :: Symbol -> String -> Int -> Maybe Tree
createNodeFromFunction [] _ _ = Nothing
createNodeFromFunction _ [] _ = Nothing
createNodeFromFunction (_:xs) str 0 = Just (Node xs Nothing Nothing)
createNodeFromFunction (_:xs) str 1 = Just (Node xs (textToAST str)
    (checkNextToParse textToAST (nextToParse str)))
createNodeFromFunction (_:xs) str 2 =
    Just (Node xs (textToAST str) (createVariadic (nextToparse str)))

stringIsBool :: String -> Bool
stringIsBool "#t" = True
stringIsBool "#f" = True
stringIsBool _ = False

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
                          createNodeFromFunction split str (countAtoms (nextToParse str) 0)
                       | otherwise = Just (Leaf (Symbol split))

textToAST :: String -> Maybe Tree
textToAST [] = Nothing
textToAST (x:xs) | skipableChar x = textToAST xs
                 | otherwise = treeFromAtom (takeWhile notSkipableChar (x:xs)) (dropWhile notSkipableChar (x:xs))
