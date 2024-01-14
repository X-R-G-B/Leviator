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
    parseInstructions,
    parseInstruction,
    parseFunction,
    parseReturn,
    parseDeclaration,
    parseAssignation,
    parseCond,
    parseWhile,
    -- Function
    parseFuncDeclaration
) where

import Control.Applicative

import AST

import Parser
import ParseUtil
import ShuntingYard
import Lexeme

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

parseVarName :: Parser Symbol
parseVarName =
    f
        <$> parseAnyChar alphabetLower
            <*> many (parseAnyChar (alphabet ++ digit ++ "_"))
    where
        f fstChar restName = fstChar : restName

parseVar :: Parser Value
parseVar = Var <$> parseVarName

parseVoid :: Parser Value
parseVoid = f <$> parseString "Void"
    where
        f _ = Void

parseOperatorFstVal :: Parser Value
parseOperatorFstVal = parseValueWithoutOperator

parseOperatorOp :: Parser Value
parseOperatorOp =
    Var
        <$> (parseString "+" <|> parseString "-" <|> parseString "*"
            <|> parseString "/" <|> parseString "{" <|> parseString "}"
            <|> parseString "==" <|> parseString "!=" <|> parseString "<=" 
            <|> parseString ">=" <|> parseString "<" <|> parseString ">")

parseOperator' :: ShuntingYardState -> Parser ShuntingYardState
parseOperator' sys =
    (fOp <$> parseOperatorOp)
    <|> (fVal <$> parseOperatorFstVal)
    where
        fVal val = shuntingYardValue val sys
        fOp op = shuntingYardOp op sys

parseOperatorTransformOne' :: [Value] -> Maybe [Value]
parseOperatorTransformOne' (x1:x2:(Var op):rest)
    | isOperator op = Just (FuncValue (op, [x1, x2]) : rest)
    | otherwise = (\ys -> x1:x2:ys) <$> parseOperatorTransformOne rest
parseOperatorTransformOne' _ = Nothing

parseOperatorTransformOne :: [Value] -> Maybe [Value]
parseOperatorTransformOne [] = Just []
parseOperatorTransformOne [x] = Just [x]
parseOperatorTransformOne [_, _] = Nothing
parseOperatorTransformOne (x1:(Var op):rest)
    | isOperator op = Nothing
    | otherwise = parseOperatorTransformOne' (x1 : Var op : rest)
parseOperatorTransformOne (x1:x2:(Var op):rest) =
    parseOperatorTransformOne' (x1 : x2 : Var op : rest)
parseOperatorTransformOne (x:xs) = (x :) <$> parseOperatorTransformOne xs

parseOperatorTransform :: [Value] -> Maybe Value
parseOperatorTransform [] = Nothing
parseOperatorTransform vals =
    case parseOperatorTransformOne vals of
        Nothing -> Nothing
        Just [] -> Nothing
        Just [x] -> Just x
        Just (x:rest) -> parseOperatorTransform (x:rest)

parseOperatorS :: ShuntingYardState -> Parser ShuntingYardState
parseOperatorS sys = Parser f
    where
        f str = case runParser (parseOperator' sys) str of
            Nothing -> Just (sys, str)
            Just (x, xs) -> case runParser (parseOperatorS x) xs of
                Nothing -> Just (x, xs)
                Just (y, ys) -> Just (y, ys)

parseOperator :: Parser Value
parseOperator = Parser f
    where
        f str = case runParser (parseOperatorS (SYS [] [])) str of
            Nothing -> Nothing
            Just (x, xs) -> pat (shuntingYardEnd x) xs
        pat (SYS _ vals) str = case parseOperatorTransform vals of
            Nothing -> Nothing
            Just x -> Just (x, str)

parseValue :: Parser Value
parseValue =
    parseOperator
    <|> parseValueWithoutOperator

parseValueWithoutOperator :: Parser Value
parseValueWithoutOperator =
    parseFuncValue
    <|> parseBoolean
    <|> parseVoid
    <|> parseInteger
    <|> parseStringView
    <|> parseCharacter
    <|> parseVar

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
parseReturn = Return <$> (parseString "<-" *> parseValue)

parseType :: Parser String
parseType =
    parseString "Bool"
    <|> parseString "Int"
    <|> parseString "Char"
    <|> parseString "Void"
    <|> parseString "StringView"

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
        f str = case runParser (parseVar <* parseString "=") str of
            Nothing -> Nothing
            Just (Var x, xs) ->
                case runParser parseValue xs of
                    Nothing -> Nothing
                    Just (y, ys) -> Just (Assignation (x, y), ys)
            _notVar -> Nothing

parseCondComp :: Parser Value
parseCondComp = parseString "if(" *> parseValue <* parseString ")"

parseCondIf :: Parser [Instruction]
parseCondIf = parseString "{" *> parseInstructions <* parseString "}"

parseCondElse :: Parser [Instruction]
parseCondElse = parseString "else{" *> parseInstructions <* parseString "}"

parseCond' :: Value -> [Instruction] -> Parser Instruction
parseCond' val ifBlock = Parser f
    where
        f (';':str) = Just (Cond (val, ifBlock, []), ';':str)
        f str = case runParser parseCondElse str of
            Nothing -> Nothing
            Just (elseBlock, ys) -> Just (Cond (val, ifBlock, elseBlock), ys)

parseCond :: Parser Instruction
parseCond = Parser f
    where
        f str = case runParser parseCondComp str of
            Nothing -> Nothing
            Just (val, xs) ->
                case runParser parseCondIf xs of
                    Nothing -> Nothing
                    Just (ifBlock, ys) -> runParser (parseCond' val ifBlock) ys

parseWhileComp :: Parser Value
parseWhileComp = parseString "while(" *> parseValue <* parseString ")"

parseWhileBlock :: Parser [Instruction]
parseWhileBlock = parseString "{" *> parseInstructions <* parseString "}"

parseWhile :: Parser Instruction
parseWhile = While <$> ((,) <$> parseWhileComp <*> parseWhileBlock)

parseInstruction :: Parser Instruction
parseInstruction =
    (parseCond
    <|> parseWhile
    <|> parseReturn
    <|> parseDeclaration
    <|> parseAssignation
    <|> parseFunction
    ) <* parseString ";"

parseInstructions :: Parser [Instruction]
parseInstructions = some parseInstruction

parseFuncVar :: Parser Var
parseFuncVar = Parser f
    where
        f str = case runParser (parseVar <* parseString ":") str of
            Nothing -> Nothing
            Just (Var x, xs) -> runParser (lmbda x <$> parseType) xs
            _notVar -> Nothing
        lmbda var typ = (var, typ)

parseFuncVars :: Parser [Var]
parseFuncVars =
    parseChar '(' *>
    many
        (parseFuncVar
            <* (parseString "," <|> parseString " ," <|> parseString ", ")
        <|> parseFuncVar)
    <* parseChar ')'

parseFuncName :: Parser (IsFuncExport, Symbol)
parseFuncName =
    ((\x -> (True, x)) <$> (parseString "export fn " *> parseVarName))
    <|> ((\x -> (False, x)) <$> (parseString "fn " *> parseVarName))

parseFuncType :: Parser Type
parseFuncType = parseString "->" *> parseType <* parseString "{"

parseFuncPrototype :: Parser FuncPrototype
parseFuncPrototype =
    f
        <$> parseFuncName
            <*> parseFuncVars
            <*> parseFuncType
    where
        f (isExport, name) vars funcType = (isExport, name, vars, funcType)

parseFuncDeclaration' :: Parser FuncDeclaration
parseFuncDeclaration' =
    (,)
        <$> parseFuncPrototype
            <*> parseInstructions
            <* parseString "};"

parseFuncDeclaration :: Parser FuncDeclaration
parseFuncDeclaration = Parser f
    where
        f str = runParser parseFuncDeclaration' (lexeme1 str)
