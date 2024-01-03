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
    parseVar,
    parseFuncValue,
    parseBoolean,
    parseInteger,
    parseCharacter,
    parseStringView,
    parseVoid,
    -- Instruction
    parseInstruction,
    parseFunction,
    parseReturn,
    parseDeclaration,
    parseAssignation,
    parseCond,
) where

import Control.Applicative

import AST

import Parser
import ParseUtil

parseBoolean :: Parser Value
parseBoolean =
    ((\_ -> Boolean True) <$> parseString "True")
    <|> ((\_ -> Boolean False) <$> parseString "False")

parseInteger :: Parser Value
parseInteger = Integer <$> parseInt

parseFuncValue :: Parser Value
parseFuncValue = Parser f
    where
        f str = case runParser parseCall str of
            Nothing -> Nothing
            Just (Function x, xs) -> Just (FuncValue x, xs)
            _notAFunction -> Nothing

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

parseVar :: Parser Value
parseVar = Parser f
    where
        f str = case runParser (parseAnyChar alphabetLower) str of
            Nothing -> Nothing
            Just (x, xs) ->
                case runParser
                    (many (parseAnyChar (alphabet ++ digit ++ "_")))
                    xs
                of
                    Nothing -> Nothing
                    Just (y, ys) -> Just (Var (x:y), ys)

parseVoid :: Parser Value
parseVoid = f <$> parseString "Void"
    where
        f _ = Void

parseValue :: Parser Value
parseValue =
    parseBoolean
    <|> parseInteger
    <|> parseCharacter
    <|> parseStringView
    <|> parseVar
    <|> parseVoid

parseCallName :: Parser Symbol
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
            Just (x, ',':' ':xs) -> Just (x, xs)
            Just (x, ',':xs) -> Just (x, xs)
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
parseCall = f <$> parseCallName <*> parseCallArgs
    where
        f name args = Function (name, args)

parseFunction :: Parser Instruction
parseFunction = parseCall

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

parseDeclaration :: Parser Instruction
parseDeclaration = Parser f
    where
        f str = case
                runParser (parseChar '@' *> parseType <* parseChar ' ') str
            of
                Nothing -> Nothing
                Just (typ, xs) ->
                    case runParser parseAssignation xs of
                        Nothing -> Nothing
                        Just (Assignation (name, val), ys) -> Just (Declaration ((name, typ), val), ys)
                        _notAssignation -> Nothing

parseAssignation :: Parser Instruction
parseAssignation = Parser f
    where
        f str = case runParser (parseVar <* parseString " = ") str of
            Nothing -> Nothing
            Just (Var x, xs) ->
                case runParser parseValue xs of
                    Nothing -> Nothing
                    Just (y, ys) -> Just (Assignation (x, y), ys)
            _notVar -> Nothing

parseCondComp :: Parser Value
parseCondComp = parseString "if (" *> parseValue <* parseString ")\n"

parseCondIf :: Parser [Instruction]
parseCondIf = parseString "{\n" *> parseInstructions <* parseString "}"

parseCondElse :: Parser [Instruction]
parseCondElse = parseString ";\nelse\n{\n" *> parseInstructions <* parseString "};\n"

parseCond :: Parser Instruction
parseCond = Parser f
    where
        f str = case runParser parseCondComp str of
            Nothing -> Nothing
            Just (val, xs) ->
                case runParser parseCondIf xs of
                    Nothing -> Nothing
                    Just (ifBlock, ys) ->
                        case runParser parseCondElse ys of
                            Nothing -> Just (Cond (val, ifBlock, []), ys)
                            Just (elseBlock, zs) ->
                                Just (Cond (val, ifBlock, elseBlock), zs)

parseInstruction :: Parser Instruction
parseInstruction =
    (parseFunction
    <|> parseReturn
    <|> parseDeclaration
    <|> parseAssignation
    <|> parseCond) <* parseString ";\n"

parseInstructions :: Parser [Instruction]
parseInstructions = some parseInstruction
