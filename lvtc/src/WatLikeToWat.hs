{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ShuntingYard
-}

module WatLikeToWat
(
    watLikeToWat
    , watsLikeToWat
) where

import Data.Int (Int32)

import WatAST
import WatLike
import AST
import Builtins

typeStringToType :: Symbol -> WatAST.Type
typeStringToType "Int" = I32
typeStringToType _ = error "Unknown type"

paramsToTypes :: [Var] -> [WatAST.Type]
paramsToTypes [] = []
paramsToTypes ((_, t):vars) = typeStringToType t : paramsToTypes vars

nameIsInParams :: String -> [Var] -> Bool
nameIsInParams _ [] = False
nameIsInParams name ((name', _):xs)
    | name == name' = True
    | otherwise = nameIsInParams name xs

findTypeFromInstructions :: String -> [Instruction] -> WatAST.Type
findTypeFromInstructions name [] = error ("Type not found for: " ++ name)
findTypeFromInstructions name ((Declaration ((name', typ), _)):xs)
    | name == name' = typeStringToType typ
    | otherwise = findTypeFromInstructions name xs
findTypeFromInstructions name (_:xs) = findTypeFromInstructions name xs

varsToDecl :: [Index] -> [Instruction] -> [Var] -> [(WatAST.Type, Int32)]
varsToDecl [] _ _ = []
varsToDecl ((nameIndex, _):xs) ins params
    | nameIsInParams ind params = varsToDecl xs ins params
    | otherwise =
        (findTypeFromInstructions ind ins, 1) : varsToDecl xs ins params
    where
        ind = show nameIndex

groupVarsToDecl :: [(WatAST.Type, Int32)] -> [(WatAST.Type, Int32)]
groupVarsToDecl [] = []
groupVarsToDecl [x] = [x]
groupVarsToDecl ((t, nb):(t', nb'):vars)
    | t == t' = groupVarsToDecl ((t, nb + nb'):vars)
    | otherwise = (t, nb) : groupVarsToDecl ((t', nb'):vars)

valueToWat :: Value -> [OpCode]
valueToWat (Var name) =
    [
        LocalGet (read name :: Int32)
    ]
valueToWat (Integer value) =
    [
        I32Const value
    ]
valueToWat (FuncValue (indexName, values)) =
    valuesToWat values
    ++ [
        Call (read indexName :: Int32)
    ]
valueToWat _ = error "value not supported"

valuesToWat :: [Value] -> [OpCode]
valuesToWat = concatMap valueToWat

instructionToWat :: Instruction -> [OpCode]
instructionToWat (AST.Return (Var indexName)) =
    [
        LocalGet (read indexName :: Int32)
        , WatAST.Return
    ]
instructionToWat (AST.Return _) = error "Return need a var"
instructionToWat (Declaration ((indexName, _), val)) =
    valueToWat val
    ++ [
        LocalSet (read indexName :: Int32)
    ]
instructionToWat (Assignation (indexName, val)) =
    valueToWat val
    ++ [
        LocalSet (read indexName :: Int32)
    ]
instructionToWat (Function (indexName, values)) =
    valuesToWat values
    ++ [
        Call (read indexName :: Int32)
    ]
instructionToWat (Cond (value, ifTrue, [])) =
    valueToWat value
    ++ [ If ]
    ++ instructionsToWat ifTrue
    ++ [ End ]
instructionToWat (Cond (value, ifTrue, ifFalse)) =
    valueToWat value
    ++ [ If ]
    ++ instructionsToWat ifTrue
    ++ [ Else ]
    ++ instructionsToWat ifFalse
    ++ [ End ]

instructionsToWat :: [Instruction] -> [OpCode]
instructionsToWat = concatMap instructionToWat
--
-- instructionsToWat = foldr ((++) . instructionToWat) []
--
-- instructionsToWat xs = foldr ((++) . instructionToWat) [] xs
--
-- instructionsToWat [] = []
-- instructionsToWat (x:xs) = instructionToWat x ++ instructionsToWat xs

watLikeToWat :: FuncDeclare -> FuncDef
watLikeToWat (((isExp, fName, params, returnType), ins), vars, originName)
    | isBuiltinsFunc fName = getBuiltinWat fName
    | otherwise = FuncDef isExp originName indexName paramType retType opcodes varsDecl
    where
        indexName = read fName :: Int32
        paramType = paramsToTypes params
        retType = typeStringToType returnType
        varsDecl = groupVarsToDecl $ varsToDecl vars ins params
        opcodes = instructionsToWat ins

watsLikeToWat :: [FuncDeclare] -> [FuncDef]
watsLikeToWat = map watLikeToWat
