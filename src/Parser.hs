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
        parseOr,
        parseAnd,
        parseAndWith,
        parseMany,
        parseSome,
        parseUInt,
        parseInt,
        parsePair,
        parseList,
    ) where

-- type Parser a = String -> Maybe (a, String)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

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

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser f
    where
        f str = case runParser p1 str of
                    Just (x, xs) -> Just (x, xs)
                    Nothing -> runParser p2 str

parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd p1 p2 = Parser f
    where
        f str = case runParser p1 str of
                    Just (x, xs) ->
                        case runParser p2 xs of
                            Just (y, ys) -> Just ((x,y), ys)
                            Nothing -> Nothing
                    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser f'
    where
        f' str = case runParser (parseAnd p1 p2) str of
                    Just ((x, y), xs) -> Just (f x y, xs)
                    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser f
    where
        f str = case runParser p str of
                    Just (x, xs) ->
                        case runParser (parseMany p) xs of
                            Just (ys, zs) -> Just (x:ys, zs)
                            Nothing -> Just ([x], xs)
                    Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser f
    where
        f str = case runParser p str of
                    Just (x, xs) ->
                        case runParser (parseMany p) xs of
                            Just (ys, zs) -> Just (x:ys, zs)
                            Nothing -> Just ([x], xs)
                    Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser f
    where
        f [] = Nothing
        f str = case runParser (parseSome (parseAnyChar "0123456789")) str of
                    Just (x, xs) -> Just (read x, xs)
                    Nothing -> Nothing

parseSign :: Parser Int
parseSign = Parser f
    where
        f str = case runParser (parseMany (parseAnyChar "+-")) str of
            Just (x, xs) | even (length (filter (== '-') x)) -> Just (1, xs)
                         | otherwise -> Just (-1, xs)
            Nothing -> Nothing

parseInt :: Parser Int
parseInt = parseAndWith (*) parseSign parseUInt

parsePair :: Parser a -> Parser b -> Parser (a,b)
parsePair p1 p2 = Parser f
    where
        f str = case runParser (parseChar '(') str of
                    Just (_, xs) ->
                        case runParser p1 xs of
                            Just (x, xs') ->
                                case runParser (parseMany (parseChar ' ')) xs' of
                                    Just (_, xs'') ->
                                        case runParser p2 xs'' of
                                            Just (y, ys) ->
                                                case runParser (parseChar ')') ys of
                                                    Just (_, ys') -> Just ((x,y), ys')
                                                    Nothing -> Nothing
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
                            case runParser (parseMany (parseChar ' ')) xs of
                                Just (_, xs') ->
                                    case runParser (parseList' p) xs' of
                                        Just (ys, ys') -> Just (x:ys, ys')
                                        Nothing -> Nothing
                                Nothing -> Nothing
                        Nothing -> Nothing

parseList :: Parser a -> Parser [a]
parseList p = Parser f
    where
        f str = case runParser (parseChar '(') str of
                    Just (_, xs) -> runParser (parseList' p) xs
                    Nothing -> Nothing
