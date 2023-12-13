{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module ComputeAST
    (
        computeAST
    ) where

import Types
import Defines
import Errors
import Functions

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

-- Compute nested lists
handleDeepList :: Env -> [Tree] -> (Env, Maybe Result)
handleDeepList env list
    | not (doesListContainsList list) = handleSimpleList env list
    | otherwise =
        case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolvedList) -> handleDeepList newEnv resolvedList



















doesListContainsList :: [Tree] -> Bool
doesListContainsList [] = False
doesListContainsList (List _ : _) = True
doesListContainsList (_ : rest) = doesListContainsList rest

--[(List [Symbol "define", Symbol "add", List [Symbol "lambda", List [Symbol "a", Symbol "b" ], List [Symbol "+", Symbol "a", Symbol "b"]]]), (List [Symbol "add", Number 1, Number 2])]
--[(List [Symbol "define", Symbol "func", List [Symbol "lambda", List [Symbol "a", Symbol "b" ], List [Symbol "define", Symbol "foo", Symbol "a"], List [Symbol "+", Symbol "foo", Symbol "b"]]]), (List [Symbol "func", Number 1, Number 2])]

--data Function = Function {
--    name :: String,
--    params :: [String],
--    body :: Tree
--} deriving (Show)





replaceSymbol :: Tree -> String -> Tree -> Tree
replaceSymbol (List lst) toReplace with =
    List (map (\t -> if t == Symbol toReplace then with else replaceSymbol t toReplace with) lst)
replaceSymbol t _ _ = t

replaceFunctionParams :: Env -> [String] -> Tree -> [Tree] -> (Env, Maybe Tree)
replaceFunctionParams env params body args
    | length params /= length args = (registerError env "Mismatched number of arguments", Nothing)
    | otherwise =
        let replacement = zip params args
            replacedBody = foldl (\b (p, a) -> replaceSymbol b p a) body replacement
        in (env, Just replacedBody)


computeFunction :: Env -> Function -> [Tree] -> (Env, Maybe Result)
computeFunction env (Function name params body) args =
    case replaceFunctionParams env params body args of
        (newEnv, Nothing) -> (newEnv, Nothing)
        (newEnv, Just replaced) -> computeAST newEnv replaced

-- Compute simple lists (no nested lists)
handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "div" : rest) = division env rest
handleSimpleList env (Symbol "mod" : rest) = modulo env rest
handleSimpleList env (Symbol smbl : rest)
    | isAFunction env smbl = case getFunctionByName env smbl of
        Just func -> computeFunction env func rest
        Nothing   -> (registerError env ("Function " ++ smbl ++ " not found"), Nothing)
    | otherwise = (registerError env ("Symbol " ++ smbl ++ " not found"), Nothing)
handleSimpleList env _ = (registerError env "Bad function call", Nothing)





















-- Handle AST that doesn't contain a list
handleNoList :: Env -> Tree -> (Env, Maybe Result)
handleNoList env (Number nbr) = (env, Just (Number nbr))
handleNoList env (Boolean value) = (env, Just (Boolean value))
handleNoList env (Symbol smbl)
    | Nothing <- value = (env, Nothing)
    | Just (List list) <- value = computeAST env (List list)
    | Just result <- value = (env, Just result)
        where (_, value) = getSymbolValue env smbl
handleNoList env _ = (env, Nothing)

-- Handle AST that register a define
handleDefine :: Env -> Tree -> (Env, Maybe Result)
handleDefine env (List [Symbol _, Symbol smbl, List [Symbol "lambda", List params, List body]])
    = (registerFunction env smbl (List params) (List body), Nothing)
handleDefine env (List [Symbol _, Symbol smbl, expr])
    = (registerDefine env smbl expr, Nothing)
handleDefine env _ = (registerError env "Bad define", Nothing)

-- Compute entire AST
computeAST :: Env -> Tree -> (Env, Maybe Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
computeAST env (List list)
    | doesListContainsList list = handleDeepList env list
    | otherwise = handleSimpleList env list
computeAST env tree = handleNoList env tree
