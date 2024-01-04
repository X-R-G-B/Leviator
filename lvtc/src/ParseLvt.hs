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

lexeme :: String -> String
lexeme [] = []
lexeme ('(':' ':xs) = lexeme ('(':xs)
lexeme (',':' ':xs) = lexeme (',':xs)
lexeme ('\n':' ':xs) = lexeme ('\n':xs)
lexeme ('e':'l':'s':'e':' ':xs) = lexeme ("else" ++ xs)
lexeme (x:xs) = x : lexeme xs

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
                    (many (parseAnyChar (alphabet ++ digit ++ "_"))) xs
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
parseCallName =
    f
        <$> parseAnyChar alphabetLower
            <*> many (parseAnyChar (alphabet ++ digit ++ "_"))
            <* parseChar '('
    where
        f fstChar restName = fstChar : restName

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
        f ('B':'o':'o':'l':xs) = Just ("Bool", xs)
        f ('I':'n':'t':xs) = Just ("Int", xs)
        f ('C':'h':'a':'r':xs) = Just ("Char", xs)
        f ('V':'o':'i':'d':xs) = Just ("Void", xs)
        f ('S':'t':'r':'i':'n':'g':'V':'i':'e':'w':xs) =
            Just ("StringView", xs)
        f _ = Nothing

parseDeclaration' :: String -> Parser Instruction
parseDeclaration' typ = Parser f
    where
        f str = case runParser parseAssignation str of
            Nothing -> Nothing
            Just (Assignation (name, val), xs) ->
                Just (Declaration ((name, typ), val), xs)
            _notAssignation -> Nothing

parseDeclaration :: Parser Instruction
parseDeclaration = Parser f
    where
        f str = case
                runParser (parseChar '@' *> parseType <* parseChar ' ') str
            of
                Nothing -> Nothing
                Just (typ, xs) -> runParser (parseDeclaration' typ) xs

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
parseCondElse = parseString "else\n{\n" *> parseInstructions <* parseString "}"

parseCond' :: Value -> [Instruction] -> Parser Instruction
parseCond' val ifBlock = Parser f
    where
        f ('\n':xs) = case runParser parseCondElse xs of
            Nothing -> Nothing
            Just (elseBlock, ys) -> Just (Cond (val, ifBlock, elseBlock), ys)
        f str = Just (Cond (val, ifBlock, []), str)

parseCond :: Parser Instruction
parseCond = Parser f
    where
        f str = case runParser parseCondComp (lexeme str) of
            Nothing -> Nothing
            Just (val, xs) ->
                case runParser parseCondIf xs of
                    Nothing -> Nothing
                    Just (ifBlock, ys) -> runParser (parseCond' val ifBlock) ys

parseInstruction :: Parser Instruction
parseInstruction =
    (parseCond
    <|> parseReturn
    <|> parseDeclaration
    <|> parseAssignation
    <|> parseFunction
    ) <* parseString ";\n"

parseInstructions :: Parser [Instruction]
parseInstructions = some parseInstruction
