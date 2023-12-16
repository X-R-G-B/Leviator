{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module ComputeAST
    (
        computeAST,
        evaluateSymbol
    ) where

import Types
import ListContainList
import Defines
import Errors
import ReplaceFunctionParams
import Functions

-- Compute a "+ - div * mod" list, using defines if needed

addition :: Env -> [Tree] -> (Env, Maybe Result)
addition env [Number a, Number b] = (env, Just (Number (a + b)))
addition env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Just (Number (a + symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Just (Number (symbolValue + b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Just (Number (symbolValueA + symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env list
    | length list /= 2 = (registerError env "Addition need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in addition", Nothing)

multiplication :: Env -> [Tree] -> (Env, Maybe Result)
multiplication env [Number a, Number b] = (env, Just (Number (a * b)))
multiplication env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Just (Number (a * symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Just (Number (symbolValue * b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Just (Number (symbolValueA * symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env list
    | length list /= 2 = (registerError env "* need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in multiplication", Nothing)

subtraction :: Env -> [Tree] -> (Env, Maybe Result)
subtraction env [Number a, Number b] = (env, Just (Number (a - b)))
subtraction env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Just (Number (a - symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Just (Number (symbolValue - b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Just (Number (symbolValueA - symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env list
    | length list /= 2 = (registerError env "- need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in subtraction", Nothing)

division :: Env -> [Tree] -> (Env, Maybe Result)
division env [Number a, Number b]
    | b == 0 = (registerError env "Division by 0", Nothing)
    | otherwise = (env, Just (Number (a `div` b)))
division env [Symbol a, Number b]
    | b == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Just (Number (symbolValue `div` b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b
    , symbolValue == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Just (Number (a `div` symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b
    , symbolValueB == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Just (Number (symbolValueA `div` symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env list
    | length list /= 2 = (registerError env "/ need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in division", Nothing)

modulo :: Env -> [Tree] -> (Env, Maybe Result)
modulo env [Number a, Number b]
    | b == 0 = (registerError env "Modulo by 0", Nothing)
    | otherwise = (env, Just (Number (a `mod` b)))
modulo env [Symbol a, Number b]
    | b == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Just (Number (symbolValue `mod` b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b
    , symbolValue == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Just (Number (a `mod` symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b
    , symbolValueB == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Just (Number (symbolValueA `mod` symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env list
    | length list /= 2 = (registerError env "% need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in modulo", Nothing)



----------------------------------------------------------------------------------



evaluateSymbol :: Env -> Symbol -> (Env, Maybe Tree)
evaluateSymbol env smbl =
    case getSymbolValue env smbl of
        (_, Nothing) -> (env, Nothing)
        (_, Just (Number number)) -> (env, Just (Number number))
        (_, Just (Boolean value)) -> (env, Just (Boolean value))
        (_, Just (List list)) -> computeAST env (List list)
        (_, _) -> (env, Nothing)



----------------------------------------------------------------------------------



-- Find nested lists and resolve them
resolveNestedLists :: Env -> [Tree] -> [Tree] -> (Env, Maybe [Tree])
resolveNestedLists env resolvedList [] = (env, Just resolvedList)
resolveNestedLists env resolvedList (List list : rest)
    | not (doesListContainsList list) =
        case handleSimpleList env list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolved) ->
                resolveNestedLists newEnv (resolvedList ++ [resolved]) rest
    | otherwise = case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just rvd)
                -> resolveNestedLists newEnv (resolvedList ++ [List rvd]) rest
resolveNestedLists env resolvedList (Number number : rest) =
    resolveNestedLists env (resolvedList ++ [Number number]) rest
resolveNestedLists env resolvedList (Boolean value : rest) =
    resolveNestedLists env (resolvedList ++ [Boolean value]) rest
resolveNestedLists env resolvedList (Symbol smbl : rest) =
    resolveNestedLists env (resolvedList ++ [Symbol smbl]) rest







----------------------------------------------------------------------------------





-- Compute simple lists (no nested lists)
handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "div" : rest) = division env rest
handleSimpleList env (Symbol "mod" : rest) = modulo env rest
handleSimpleList env (Symbol smbl : rest) =
    case getFunctionByName env smbl of
        Nothing -> (registerError env ("Function " ++ smbl ++ " not found"), Nothing)
        Just func ->
            case computeFunction env func rest of
                (_, Just res) -> (env, Just res)
                (newEnv, Nothing) -> (env { errors = errors newEnv }, Nothing)
handleSimpleList env _ = (registerError env "Bad function call", Nothing)

-- Compute nested lists
handleDeepList :: Env -> [Tree] -> (Env, Maybe Result)
handleDeepList env list
    | not (doesListContainsList list) = handleSimpleList env list
    | otherwise =
        case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolvedList) -> handleDeepList newEnv resolvedList


----------------------------------------------------------------------------------


handleLambda :: Env -> Tree -> (Env, Maybe Result)
handleLambda env (List (List (Symbol "lambda" : List fnParams : fnBodies): (List args): _))
    = computeFunction env (Function "" (getParams (List fnParams)) fnBodies) args
handleLambda env _ = (registerError env "Bad lambda", Nothing)



-------------------------------------------------------------------------------------



computeFunctionBody :: Env -> Function -> [Tree] -> (Env, Maybe Result)
computeFunctionBody env (Function _ _ []) _ = (env, Nothing)
computeFunctionBody env (Function _ fnParams (x:_)) args =
    case replaceFunctionParams env fnParams x args of
        (newEnv, Nothing) -> (newEnv, Nothing)
        (newEnv, Just replaced) -> computeAST newEnv replaced

computeFunction :: Env -> Function -> [Tree] -> (Env, Maybe Result)
computeFunction env (Function fnName fnParams (x:xs:rest)) args =
    case computeFunctionBody env (Function fnName fnParams [x]) args of
        (newEnv, Nothing) -> computeFunction newEnv (Function fnName fnParams (xs:rest)) args
        (_, Just _) -> (registerError env "Return needs to be the last statement", Nothing)
computeFunction env (Function fnName fnParams (x:_)) args =
    case computeFunctionBody env (Function fnName fnParams [x]) args of
        (newEnv, Nothing) -> (registerError newEnv "Missing return in function", Nothing)
        (newEnv, Just replaced) -> computeAST newEnv replaced
computeFunction env _ _ = (registerError env "Bad function call", Nothing)



-------------------------------------------------------------------------------------



-- Compute AST that doesn't contain a list
computeASTWithoutList :: Env -> Tree -> (Env, Maybe Result)
computeASTWithoutList env (Number nbr) = (env, Just (Number nbr))
computeASTWithoutList env (Boolean value) = (env, Just (Boolean value))
computeASTWithoutList env (Symbol smbl)
    | Nothing <- value = (env, Nothing)
    | Just (List list) <- value = computeAST env (List list)
    | Just result <- value = (env, Just result)
        where (_, value) = getSymbolValue env smbl
computeASTWithoutList env _ = (env, Nothing)

-- Compute entire AST
computeAST :: Env -> Tree -> (Env, Maybe Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
computeAST env tree@(List (List (Symbol "lambda" : _) : _)) = handleLambda env tree
computeAST env (List list)
    | doesListContainsList list = handleDeepList env list
    | otherwise = handleSimpleList env list
computeAST env tree = computeASTWithoutList env tree
