{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ParseLvt
-}

module ParseLvt
(
    -- Value
    parseValue,
    Value (..),
    parseBoolean,
    parseInteger,
    parseCharacter,
    parseStringView,
    parseVariable,
    parseVoid,
    -- Instruction
    parseInstruction,
    Instruction (..),
    parseCall,
    parseReturn,
    parseDeclare,
    parseAssign,
) where

import Parser
import ParseUtil
import Control.Applicative
import Data.Int (Int32)

data Value =
    Boolean Bool
    | Integer Int32
    | Character Char
    | StringView String
    | Variable String
    | Void

instance Show Value where
    show (Boolean x) = "B< " ++ show x ++ " >"
    show (Integer x) = "I< " ++ show x ++ " >"
    show (Character x) = "C< " ++ show x ++ " >"
    show (StringView x) = "SV< " ++ show x ++ " >"
    show (Variable x) = "V< " ++ show x ++ " >"
    show Void = "Void"

instance Eq Value where
    (==) (Boolean x) (Boolean y) = x == y
    (==) (Integer x) (Integer y) = x == y
    (==) (Character x) (Character y) = x == y
    (==) (StringView x) (StringView y) = x == y
    (==) (Variable x) (Variable y) = x == y
    (==) _ _ = False

data Instruction =
    -- Call Name [Arguments]
    Call String [Value]
    -- Return Value
    | Return Value
    -- Declare Type Name Value
    | Declare String String Value
    -- Assign Name Value
    | Assign String Value

instance Show Instruction where
    show (Call x y) =
        "Call[< " ++ show x ++ " >< " ++ show y ++ " >]"
    show (Return x) =
        "Return[< " ++ show x ++ " >]"
    show (Declare x y z) =
        "Declare[< " ++ show x ++ " >< " ++ show y ++ " >< " ++ show z ++ " >]"
    show (Assign x y) =
        "Assign[< " ++ show x ++ " >< " ++ show y ++ " >]"

instance Eq Instruction where
    (==) (Call x y) (Call a b) = x == a && y == b
    (==) (Return x) (Return y) = x == y
    (==) (Declare x y z) (Declare a b c) = x == a && y == b && z == c
    (==) (Assign x y) (Assign a b) = x == a && y == b
    (==) _ _ = False

parseBoolean :: Parser Value
parseBoolean =
    ((\_ -> Boolean True) <$> parseString "True")
    <|> ((\_ -> Boolean False) <$> parseString "False")

parseInteger :: Parser Value
parseInteger = Integer <$> parseInt

parseCharacter :: Parser Value
parseCharacter =
    Character <$>
        (
        parseChar '\''
        *> parseAnyChar (alphabet ++ digit ++ special)
        <* parseChar '\''
        )

parseStringView :: Parser Value
parseStringView =
    StringView <$>
        (
        parseChar '\"'
        *> parseAllCharUntil "\""
        )

parseVariable :: Parser Value
parseVariable = Parser f
    where
        f str = case runParser (parseAnyChar alphabetLower) str of
            Nothing -> Nothing
            Just (x, xs) ->
                case runParser
                    (many (parseAnyChar (alphabet ++ digit ++ "_")))
                    xs
                of
                    Nothing -> Nothing
                    Just (y, ys) -> Just (Variable (x:y), ys)

parseVoid :: Parser Value
parseVoid = (\_ -> Void) <$> parseString "Void"

parseValue :: Parser Value
parseValue =
    parseBoolean
    <|> parseInteger
    <|> parseCharacter
    <|> parseStringView
    <|> parseVariable
    <|> parseVoid

parseCallName :: Parser String
parseCallName = Parser f
    where
        f str = case runParser (parseAnyChar alphabetLower) str of
            Nothing -> Nothing
            Just (x, xs) ->
                case runParser
                    (many (parseAnyChar (alphabet ++ digit ++ "_")))
                    xs
                of
                    Nothing -> Nothing
                    Just (y, ys) ->
                        case runParser
                            (parseChar '(')
                            ys
                        of
                            Nothing -> Nothing
                            Just (_, zs) -> Just (x:y, zs)

parseCallArg :: Parser Value
parseCallArg = Parser f
    where
        f str = case runParser parseValue str of
            Nothing -> Nothing
            Just (x, (',':' ':xs)) -> Just (x, xs)
            Just (x, (',':xs)) -> Just (x, xs)
            Just (x, xs) -> Just (x, xs)

parseCallArgs :: Parser [Value]
parseCallArgs = Parser f
    where
        f (')':xs) = Just ([], xs)
        f str = case runParser parseCallArg str of
            Nothing -> Nothing
            Just (x, xs) ->
                case runParser parseCallArgs xs of
                    Nothing -> Just ([x], xs)
                    Just (y, ys) -> Just (x:y, ys)

parseCall :: Parser Instruction
parseCall = Call <$> parseCallName <*> parseCallArgs

parseReturn :: Parser Instruction
parseReturn = Return <$> (parseString "<- " *> parseValue)

parseType :: Parser String
parseType = Parser f
    where
        f ('B':'o':'o':'l':xs) =
            Just ("Bool", xs)
        f ('I':'n':'t':xs) =
            Just ("Int", xs)
        f ('C':'h':'a':'r':xs) =
            Just ("Char", xs)
        f ('S':'t':'r':'i':'n':'g':'V':'i':'e':'w':xs) =
            Just ("StringView", xs)
        f _ = Nothing

parseDeclare :: Parser Instruction
parseDeclare = Parser f
    where
        f str = case
                runParser (parseChar '@' *> parseType <* parseChar ' ') str
            of
                Nothing -> Nothing
                Just (x, xs) ->
                    case runParser parseAssign xs of
                        Nothing -> Nothing
                        Just (Assign y1 y2, ys) -> Just (Declare x y1 y2, ys)
                        _ -> Nothing

parseAssign :: Parser Instruction
parseAssign = Parser f
    where
        f str = case runParser (parseVariable <* parseString " = ") str of
            Nothing -> Nothing
            Just (Variable x, xs) ->
                case runParser parseValue xs of
                    Nothing -> Nothing
                    Just (y, ys) -> Just (Assign x y, ys)
            _ -> Nothing

parseInstruction :: Parser Instruction
parseInstruction =
    (parseCall
    <|> parseReturn
    <|> parseDeclare
    <|> parseAssign) <* parseString ";\n"
