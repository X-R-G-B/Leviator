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

import Debug.Trace
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

stringIsNumber' :: String -> Bool
stringIsNumber' [] = False
stringIsNumber' [x] | Data.Char.isDigit x = True
                    | otherwise = False
stringIsNumber' (x:xs) | Data.Char.isDigit x = stringIsNumber' xs
                       | otherwise = False

stringIsNumber :: String -> Bool
stringIsNumber [] = False
stringIsNumber [x] | Data.Char.isDigit x = True
                   | otherwise = False
stringIsNumber str =
    stringIsNumber' (takeWhile (\x -> notSkipableChar x && x /= ')') str)

parseStringNumber :: String -> Atom
parseStringNumber str =
    Number (read
        (takeWhile (\x -> notSkipableChar x && x /= ')') str)
        :: Data.Int.Int64)

popBackPrths :: String -> String
popBackPrths [] = []
popBackPrths [')'] = []
popBackPrths (x:xs) = (x:popBackPrths xs)

nextToParse' :: String -> Int -> String
nextToParse' [] _ = []
nextToParse' (')':xs) 1 = dropWhile (\x -> skipableChar x || x == ')') xs
nextToParse' (')':xs) depth = nextToParse' xs (depth - 1)
nextToParse' ('(':xs) depth = nextToParse' xs (depth + 1)
nextToParse' (_:xs) depth = nextToParse' xs depth

cutAtClose :: String -> String
cutAtClose [] = []
cutAtClose (')':_) = []
cutAtClose (x:xs) = (x:cutAtClose xs)

nextToParse :: String -> String
nextToParse [] = []
nextToParse ('(':xs) = nextToParse' xs 1
nextToParse str | skipableChar (head str) = nextToParse (dropWhile skipableChar str)
                | (last str) == ')' = nextToParse (popBackPrths str)
                | otherwise = dropWhile skipableChar
    (dropWhile notSkipableChar (dropWhile skipableChar str))

countAtoms :: String -> Int -> Int
countAtoms str depth | depth >= 2 = trace ("maxdepth") $ 2
                     | not (null $ takeWhile
                        (/= ')')
                        (dropWhile skipableChar str)) = trace ("+1atom, str: " ++ str ++ " parseRes: " ++ (nextToParse str)) $
                            countAtoms (nextToParse str) (depth + 1)
                     | otherwise = depth

createVariadic :: String -> Maybe Tree
createVariadic str =
    Just $ Variadic (textToAST str) (textToAST (nextToParse str))

createNodeFromFunction :: Symbol -> String -> String -> Int -> Maybe Tree
createNodeFromFunction [] _ _ _ = Nothing
createNodeFromFunction (_:xs) [] _ 0 = Just (Leaf (Symbol xs))
createNodeFromFunction (_:xs) str _ 0 = Just (Node xs (textToAST str) (Just Empty))
createNodeFromFunction _ [] _ _ = Nothing
createNodeFromFunction (_:xs) str tail_ 1 = Just (Node xs (textToAST str)
    (textToAST tail_))
createNodeFromFunction (_:xs) str tail_ 2 =
    Just (Node xs (textToAST str) (createVariadic tail_))
createNodeFromFunction _ _ _ _ = Nothing

stringIsBool :: String -> Bool
stringIsBool "#t" = True
stringIsBool "#f" = True
stringIsBool "#f)" = True
stringIsBool "#t)" = True
stringIsBool ('#':'t':xs) | dropWhile skipableChar xs == ")" = True
                          | otherwise = False
stringIsBool ('#':'f':xs) | dropWhile skipableChar xs == ")" = True
                          | otherwise = False
stringIsBool _ = False

createBool :: String -> Maybe Tree
createBool "#f" = Just (Leaf (Boolean False))
createBool "#t" = Just (Leaf (Boolean True))
createBool "#f)" = Just (Leaf (Boolean False))
createBool "#t)" = Just (Leaf (Boolean True))
createBool ('#':'t':xs) | dropWhile skipableChar xs == ")" =
                            Just (Leaf (Boolean True))
                        | otherwise = Nothing
createBool ('#':'f':xs) | dropWhile skipableChar xs == ")" =
                            Just (Leaf (Boolean False))
                        | otherwise = Nothing
createBool _ = Nothing

treeFromAtom :: String -> String -> Maybe Tree
treeFromAtom [] _ = Nothing
treeFromAtom split str | stringIsNumber split =
                            Just (Leaf (parseStringNumber split))
                       | stringIsBool split = createBool split
                       | isFunction split = createNodeFromFunction
                            (takeWhile (/= ')') split)
                            (cutAtClose str)
                            (nextToParse str)
                            (countAtoms (nextToParse str) 0)
                       | otherwise =
                            Just (Leaf (Symbol $ takeWhile (/= ')') split))

textToAST :: String -> Maybe Tree
textToAST [] = Nothing
textToAST (x:xs) | skipableChar x = textToAST xs
                 | otherwise = treeFromAtom
                        (takeWhile notSkipableChar (x:xs))
                        (dropWhile notSkipableChar (x:xs))
