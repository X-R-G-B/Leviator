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
stringIsNumber [x] | Data.Char.isDigit x = True
                   | otherwise = False
stringIsNumber (x:xs:xss) | Data.Char.isDigit x && xs == ')' =
                                stringIsNumber (xs:xss)
                          | Data.Char.isDigit x = True
                          | skipableChar x = stringIsNumber (xs:xss)
                          | otherwise = False

parseStringNumber :: String -> Atom
parseStringNumber str = Number (read (takeWhile (\x -> notSkipableChar x && x /= ')') str) :: Data.Int.Int64)

nextToParse' :: String -> Int -> String
nextToParse' [] _ = []
nextToParse' (')':xs) 1 = dropWhile skipableChar xs
nextToParse' (')':xs) depth = nextToParse' xs (depth - 1)
nextToParse' ('(':xs) depth = nextToParse' xs (depth + 1)
nextToParse' (_:xs) depth = nextToParse' xs depth

nextToParse :: String -> String
nextToParse [] = []
nextToParse ('(':xs) = nextToParse' xs 0
nextToParse str = dropWhile skipableChar (dropWhile notSkipableChar (dropWhile skipableChar str))

countAtoms :: String -> Int -> Int
countAtoms str depth | depth >= 2 = 2
                     | not (null str) = countAtoms (nextToParse str) (depth + 1)
                     | otherwise = depth

createVariadic :: String -> Maybe Tree
createVariadic str = Just $ Variadic (textToAST str) (textToAST (nextToParse str))

createNodeFromFunction :: Symbol -> String -> Int -> Maybe Tree
createNodeFromFunction [] _ _ = Nothing
createNodeFromFunction (_:xs) [] 0 = Just (Node xs Nothing Nothing)
createNodeFromFunction (_:xs) str 0 = Just (Node xs (textToAST str) Nothing)
createNodeFromFunction _ [] _ = Nothing
createNodeFromFunction (_:xs) str 1 = Just (Node xs (textToAST str)
    (textToAST (nextToParse str)))
createNodeFromFunction (_:xs) str 2 =
    Just (Node xs (textToAST str) (createVariadic (nextToParse str)))
createNodeFromFunction _ _ _ = Nothing

stringIsBool :: String -> Bool
stringIsBool "#t" = True
stringIsBool "#f" = True
stringIsBool ('#':'t':xs) | xs == ")" = True
                          | dropWhile skipableChar xs == ")" = True
stringIsBool ('#':'f':xs) | xs == ")" = True
                          | dropWhile skipableChar xs == ")" = True
stringIsBool _ = False

createBool :: String -> Maybe Tree
createBool "#t" = Just (Leaf (Boolean True))
createBool ('#':'t':xs) | xs == ")" = Just (Leaf (Boolean True))
                        | dropWhile skipableChar xs == ")" =
                            Just (Leaf (Boolean True))
createBool "#f" = Just (Leaf (Boolean False))
createBool ('#':'f':xs) | xs == ")" = Just (Leaf (Boolean False))
                        | dropWhile skipableChar xs == ")" =
                            Just (Leaf (Boolean False))
createBool _ = Nothing

treeFromAtom :: String -> String -> Maybe Tree
treeFromAtom [] _ = Nothing
treeFromAtom split str | stringIsNumber split =
                            Just (Leaf (parseStringNumber split))
                       | stringIsBool split = createBool split
                       | isFunction split = createNodeFromFunction
                            (takeWhile (/= ')') split)
                            str
                            (countAtoms (nextToParse str) 0)
                       | otherwise = Just (Leaf (Symbol $ takeWhile (/= ')') split))

textToAST :: String -> Maybe Tree
textToAST [] = Nothing
textToAST (x:xs) | skipableChar x = textToAST xs
                 | otherwise = treeFromAtom (takeWhile notSkipableChar (x:xs)) (dropWhile notSkipableChar (x:xs))
