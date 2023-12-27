{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Parser
-}

module Parser (
        Parser (..),
    ) where

import Control.Applicative

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
