{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Type checker
-}

module Lib
    ( someFunc
    ) where

import AST

--checkCondition :: Condition -> TypeEnv -> Maybe TypeEnv
--checkCondition (Symbol s, xs, ys) env =
--checkCondition (Type t, xs, ys) env =
--checkCondition (FuncCall (symbol, values), xs, ys) env = checkBoolFunc (checkFuncCall (FuncCall (symbol, values)) env) xs ys
--checkCondition _ _ = Nothing

findVarInParams :: Value -> [Var] -> Maybe Type
findVarInParams (Var s) ((Var s, t):xs)
  | s == s = Just t
  | otherwise = findVarInParams (Var s) xs
findVarInParams _ [] = Nothing

findVarIn

findValueType :: Value -> [Var] -> [VarAssignation] -> [FuncDeclaration] -> Maybe Type
findSymbol (Var s) params vars env = (findVarParams (Var s) params) <|> ()

checkValueType :: Type -> Maybe Type -> Bool
checkValueType _ Nothing = False
checkValueType t (Just t') = t == t'

assertTypeAndValue :: Type -> Value -> [Var] -> [VarAssignation] -> [FuncDeclaration] -> Bool
assertTypeAndValue _ (Var s) [] [] [] = False
assertTypeAndValue "Int" (Integer _) _ = True
assertTypeAndValue "Bool" (Boolean _) _ = True
assertTypeAndValue t v params vars env = checkValueType $ t (findValueType v params vars env)
assertTypeAndValue _ _ _ = False

isTypeValid :: Type -> Bool
isTypeValid "Int" = True
isTypeValid "Bool" = True
isTypeValid _ = False

checkVars :: [Value] -> Maybe [Var] -> [Var] -> [VarAssignation] -> [FuncDeclaration] -> Bool
checkVarsTypes [] _ _ _ _ = False
checkVarsTypes _ [] _ _ _ = False
checkVarsTypes values param _ _ _ | length fst /= length scd = False
checkVarsTypes [(fs v)] [(ss t)] params vars env = assertTypeAndValue t v params vars env
checkVarsTypes ((fs v):xs) ((ss t):ys) params vars env
  | assertTypeAndValue t v params vars env = checkVars xs ys params vars env
  | otherwise = False

findFunc :: Symbol -> [FuncDeclaration] -> Maybe [Var]
findFunc s [] = Nothing
findFunc s ((FuncDeclaration (FuncPrototype s params _) _):xs) | s == s = Just params
findFunc s (_:xs) = findFunc s xs

checkCall :: FuncPrototype -> FuncCall -> [VarAssignation] -> [FuncDeclaration] -> [Instructions] -> Maybe [FuncDeclaration]
checkCall (FuncPrototype s params t) (symbol, values) vars env xs
  | s == symbol && checkVars values (Just params) params vars env =
    checkInstructions (FuncPrototype s param t) xs env vars
  | otherwise = Nothing
checkCall (FuncPrototype _ params t) (s, values) vars env xs
  | checkVars values (findFunc s env) params vars env =
    checkInstructions (FuncPrototype s param t) xs env vars

checkInstructions :: FuncPrototype -> [Instructions] -> [FuncDeclaration] -> [VarAssignation] -> Maybe [FuncDeclaration]
checkInstructions prototype ((Call func):xs) env vars = checkCall prototype func vars env xs
checkInstructions prototype ((Return ret):xs) env vars = checkReturn prototype ret vars env xs
checkInstructions prototype ((Declaration declaration):xs) env vars = checkDeclaration prototype declaration vars env xs
checkInstructions prototype ((Assignation assignation):xs) env vars = checkAssignation prototype declaration vars env xs
checkInstructions prototype ((Condition assignation):xs) env vars = checkAssignation prototype declaration vars env xs
checkInstructions _ _ _ = Nothing

checkVarTypes :: [Var] -> Bool
checkVarTypes [] = False
checkVarTypes [x] = isTypeValid (snd x)
checkVarTypes (x:xs) | isTypeValid (snd x) = checkVarTypes xs

checkFunction :: FuncDeclaration -> [FuncDeclaration] -> Maybe [FuncDeclaration]
checkFunction (FuncDeclaration (FuncPrototype _ args _) _) | not (checkVarTypes args) = Nothing
checkFunction (FuncDeclaration (FuncPrototype _ _ t) _) | not (isTypeValid t) = Nothing
checkFunction (FuncDeclaration prototype instructions) env = checkInstructions prototype instructions env []

checkNotExisting :: FuncDeclaration -> [FuncDeclaration] -> Bool
checkNotExisting _ [] = True
checkNotExisting (FuncDeclaration (FuncPrototype s _ _) _) ((FuncDeclaration (FuncPrototype ls _ _) _):xs)
    | s == ls = False
    | otherwise = checkNotExisting (FuncDeclaration (FuncPrototype s _ _) _) xs

checkDeclarations :: [FuncDeclaration] -> Maybe [FuncDeclaration] -> Maybe [FuncDeclaration]
checkDeclarations _ Nothing = Nothing
checkDeclarations ((FuncDeclaration func):xs) (Just env)
  | checkNotExisting func env =
    checkDeclarations xs (checkFunction func env)
  | otherwise = Nothing
checkDeclarations _ _ = Nothing

handleTypeCheck :: [FuncDeclaration] -> Maybe [FuncDeclaration]
handleTypeCheck expressions = checkDeclarations expressions (Just [])
