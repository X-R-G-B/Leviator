{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ParseUtil
-}

module ParseUtil (
    parseChar,
    parseAnyChar,
    parseUInt,
    parseSign,
    parseInt,
    parseString,
    parseBetween,
    parseAfter,
    parseBefore,
    parseAllCharUntil,
) where

import Parser
import Data.Int (Int32)
import Control.Applicative

parseChar :: Char -> Parser Char
parseChar c = Parser f
    where
        f [] = Nothing
        f (x:xs) | x == c = Just (x, xs)
                 | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser f
    where
        f [] = Nothing
        f (x:xs) | x `elem` str = Just (x, xs)
                 | otherwise = Nothing

parseString :: String -> Parser String
parseString value = Parser f
    where
        f s | take (length value) s == value = Just (value, drop (length value) s)
                 | otherwise = Nothing

parseUInt :: Parser Int32
parseUInt = read <$> some (parseAnyChar "0123456789")

parseSign :: Parser Int32
parseSign = f <$> many (parseAnyChar "-+")
    where
        f s | even (length (filter (== '-') s)) = 1
            | otherwise = -1

parseInt :: Parser Int32
parseInt = (*) <$> parseSign <*> parseUInt

parseBetween :: Parser a -> Parser b -> Parser c -> Parser c
parseBetween open close parser = open *> parser <* close

parseAfter :: Parser a -> Parser b -> Parser b
parseAfter open parser = open *> parser

parseBefore :: Parser a -> Parser b -> Parser a
parseBefore parser close = parser <* close

parseAllCharUntil :: String -> Parser String
parseAllCharUntil str = Parser f
    where
        f [] = empty
        f (x:xs) = case runParser (parseString str) (x:xs) of
                Nothing -> runParser ((x :) <$> parseAllCharUntil str) xs
                Just (y, ys) -> Just (y, ys)
