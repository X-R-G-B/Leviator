{-
-- EPITECH PROJECT, 2023
-- Parser
-- File description:
-- Parser
-}

module Parser
    (
        Parser (Parser, runParser),
        parseChar,
        -- parseAnyChar1,
        parseAnyChar,
        parseUInt,
        parseInt,
        parsePair,
        parseList,
        parseTree
    ) where

import Types
import Control.Applicative
import Data.Int (Int64)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser f
        where
            f str = case runParser parser str of
                        Just (x, xs) -> Just (fct x, xs)
                        Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser f
        where
            f str = Just (x, str)
    p1 <*> p2 = Parser f
        where
            f str = case runParser p1 str of
                        Just (x, xs) ->
                            case runParser p2 xs of
                                Just (y, ys) -> Just (x y, ys)
                                Nothing -> Nothing
                        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser f
        where
            f _ = Nothing
    p1 <|> p2 = Parser f
        where
            f str = case runParser p1 str of
                        Just (x, xs) -> Just (x, xs)
                        Nothing -> runParser p2 str

instance Monad Parser where
    parser >>= fct = Parser f
        where
            f str = case runParser parser str of
                        Just (x, xs) -> runParser (fct x) xs
                        Nothing -> Nothing
    return = pure

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

parseUInt :: Parser Int64
parseUInt = read <$> some (parseAnyChar "0123456789")

parseSign :: Parser Int64
parseSign = Parser f
    where
        f str = case runParser (many (parseAnyChar "+-")) str of
            Just (x, xs) | even (length (filter (== '-') x)) -> Just (1, xs)
                         | otherwise -> Just (-1, xs)
            Nothing -> Nothing

parseInt :: Parser Int64
parseInt = (*) <$> parseSign <*> parseUInt

parseSymbol :: Parser Symbol
parseSymbol = some (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "*/+=-_!<>"))

stringToBool :: String -> Bool
stringToBool "#t" = True
stringToBool "#f" = False
stringToBool _ = False

parseBool :: Parser Bool
parseBool = stringToBool <$> ((:) <$> (parseChar '#') <*> ((\x -> x:[]) <$> (parseAnyChar "tf")))

parsePair :: Parser a -> Parser b -> Parser (a,b)
parsePair p1 p2 = Parser f
    where
        f str = case runParser (parseChar '(') str of
                    Just (_, xs) ->
                        case runParser p1 xs of
                            Just (x, xs') ->
                                case runParser (many (parseChar ' ')) xs' of
                                    Just (_, xs'') ->
                                        case runParser p2 xs'' of
                                            Just (x', xs''') ->
                                                runParser
                                                    ((x, x') <$ parseChar ')')
                                                    xs'''
                                            Nothing -> Nothing
                                    Nothing -> Nothing
                            Nothing -> Nothing
                    Nothing -> Nothing

parseList' :: Parser a -> Parser [a]
parseList' p = Parser f
    where
        f str = case runParser (parseChar ')') str of
                    Just (_, xs) -> Just ([], xs)
                    Nothing -> case runParser p str of
                        Just (x, xs) ->
                            case runParser (many (parseChar ' ')) xs of
                                Just (_, xs') ->
                                    runParser ((x :) <$> parseList' p) xs'
                                Nothing -> Nothing
                        Nothing -> Nothing

parseList :: Parser a -> Parser [a]
parseList p = Parser f
    where
        f str = case runParser (parseChar '(') str of
                    Just (_, xs) -> runParser (parseList' p) xs
                    Nothing -> Nothing

parseTree :: Parser Tree
parseTree = ((\x -> List x) <$> (parseList parseTree))
    <|> ((\x -> Number x) <$> (parseInt))
    <|> ((\x -> Boolean x) <$> (parseBool))
    <|> ((\x -> Symbol x) <$> (parseSymbol))
