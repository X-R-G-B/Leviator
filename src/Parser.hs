{-
-- EPITECH PROJECT, 2023
-- Parser
-- File description:
-- Parser
-}

module Parser
    (
        Parser,
        parseChar,
        parseAnyChar1,
        parseAnyChar,
        parseOr,
        parseAnd,
        parseAndWhith,
        parseMany,
        parseSome
    ) where

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar c (x:xs)  | x == c = Just (c, xs)
                    | otherwise = Nothing

parseAnyChar1 :: String -> Parser Char
parseAnyChar1 _ [] = Nothing
parseAnyChar1 anyChar (x:xs) | x `elem` anyChar = Just (x, xs)
                            | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar _ [] = Nothing
parseAnyChar [] _ = Nothing
parseAnyChar [x] str = parseChar x str
parseAnyChar (x:xx:xs) str = case parseOr (parseChar x) (parseChar xx) str of
                                Just (a, b) -> Just (a, b)
                                Nothing -> parseAnyChar xs str

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str = case p1 str of
                        Just (x, xs) -> Just (x, xs)
                        Nothing -> p2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 str = case p1 str of
                        Just (x, xs) ->
                            case p2 xs of
                                Just (x', xs') -> Just ((x, x'), xs')
                                Nothing -> Nothing
                        Nothing -> Nothing

parseAndWhith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWhith f p1 p2 str = case parseAnd p1 p2 str of
                                Just ((x, x'), xs) -> Just (f x x', xs)
                                Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p str = case p str of
                    Just (x, xs) ->
                        case parseMany p xs of
                            Just (x', xs') -> Just (x:x', xs')
                            Nothing -> Just ([x], xs)
                    Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome p str = case p str of
                    Just (x, xs) ->
                        case parseMany p xs of
                            Just (x', xs') -> Just (x:x', xs')
                            Nothing -> Just ([x], xs)
                    Nothing -> Nothing
