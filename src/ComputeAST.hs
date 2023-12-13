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

-- Find and execute user defined function

getFunctionByName :: Env -> String -> Maybe Function
getFunctionByName (Env { functions = [] }) _ = Nothing
getFunctionByName (Env { functions = (Function name params body):xs }) expr
    | name == expr = Just (Function name params body)
    | otherwise = getFunctionByName (Env { functions = xs }) expr

-- Compute a "+ - div * mod" list, using defines if needed

addition :: Env -> [Tree] -> (Env, Maybe Result)
addition env [Number a, Number b] = (env, Just (Number (a + b)))
addition env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a + symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue + b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA + symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env list
    | length list /= 2 = (registerError env "Addition need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in addition", Nothing)

multiplication :: Env -> [Tree] -> (Env, Maybe Result)
multiplication env [Number a, Number b] = (env, Just (Number (a * b)))
multiplication env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a * symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue * b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA * symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env list
    | length list /= 2 = (registerError env "* need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in multiplication", Nothing)

subtraction :: Env -> [Tree] -> (Env, Maybe Result)
subtraction env [Number a, Number b] = (env, Just (Number (a - b)))
subtraction env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a - symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue - b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
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
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue `div` b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b
    , symbolValue == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a `div` symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b
    , symbolValueB == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
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
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue `mod` b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b
    , symbolValue == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a `mod` symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b
    , symbolValueB == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA `mod` symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env list
    | length list /= 2 = (registerError env "% need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in modulo", Nothing)





















isAFunction :: Env -> String -> Bool
isAFunction (Env { functions = [] }) _ = False
isAFunction (Env { functions = (Function name _ _):xs }) expr
    | name == expr = True
    | otherwise = isAFunction (Env { functions = xs }) expr

getSymbolValueFromFunction :: Env -> String -> (Env, Maybe Tree)
getSymbolValueFromFunction env@(Env { functions = [] }) symbl = (env, Nothing)
getSymbolValueFromFunction env@(Env { functions = (Function name params bodies):xs }) symbl
    | name == symbl = case computeFunction env (Function name params bodies) [] of
            (newEnv, Nothing) -> (registerError newEnv "Function call failed", Nothing)
            (newEnv, Just result) -> (env, Just result)
    | otherwise = getSymbolValueFromFunction (Env { functions = xs }) symbl

getSymbolValueFromDefine :: Env -> String -> (Env, Maybe Tree)
getSymbolValueFromDefine env@(Env { defines = [] }) symbl = (env, Nothing)
getSymbolValueFromDefine env@(Env { defines = (Define smbl value):xs })
    symbl
    | smbl == symbl = (env, Just value)
    | otherwise = getSymbolValueFromDefine (Env { defines = xs }) symbl

getSymbolValue :: Env -> String -> (Env, Maybe Tree)
getSymbolValue env symbl
    | isAFunction env symbl = getSymbolValueFromFunction env symbl
    | otherwise = getSymbolValueFromDefine env symbl










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
            replacedbody = foldl (\acc (param, arg) -> replaceSymbol acc param arg) body replacement
        in (env, Just replacedbody)

computeFunction' :: Env -> Function -> [Tree] -> (Env, Maybe Result)
computeFunction' env (Function _ _ []) _ = (env, Nothing)
computeFunction' env (Function name params (x:_)) args =
    case replaceFunctionParams env params x args of
        (newEnv, Nothing) -> (newEnv, Nothing)
        (newEnv, Just replaced) -> computeAST newEnv replaced

computeFunction :: Env -> Function -> [Tree] -> (Env, Maybe Result)
computeFunction env (Function _ _ []) _ = (env, Nothing)
computeFunction env (Function name params (x:xs:rest)) args =
    case computeFunction' env (Function name params [x]) args of
        (newEnv, Nothing) -> computeFunction newEnv (Function name params (xs:rest)) args
        (newEnv, Just replaced) -> (registerError newEnv "Return needs to be the last statement", Nothing)
computeFunction env (Function name params (x:_)) args =
    case computeFunction' env (Function name params [x]) args of
        (newEnv, Nothing) -> (registerError newEnv "Missing return in function", Nothing)
        (newEnv, Just replaced) -> computeAST newEnv replaced
computeFunction env _ _ = (registerError env "Bad function call", Nothing)

-- computeFunction :: Env -> Function -> [Tree] -> (Env, Maybe Result)
-- computeFunction env (Function name params bodies) args =
--     case replaceFunctionParams env params bodies args of
--         (newEnv, Nothing) -> (newEnv, Nothing)
--         (newEnv, Just replaced) -> computeAST newEnv replaced

-- Compute simple lists (no nested lists)
handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "div" : rest) = division env rest
handleSimpleList env (Symbol "mod" : rest) = modulo env rest
handleSimpleList env (Symbol smbl : rest) =
    case getFunctionByName env smbl of
        Just func ->
            let (newEnv, result) = computeFunction env func rest
            in case result of
                Just res -> (env, Just res)
                Nothing -> (registerError env ("Function " ++ smbl ++ " not found"), Nothing)
        Nothing   -> (registerError env ("Function " ++ smbl ++ " not found"), Nothing)
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
handleDefine env (List [Symbol _, Symbol smbl, List (Symbol "lambda": List params : bodies)]) = (registerFunction env smbl (List params) bodies, Nothing)
handleDefine env (List [Symbol _, Symbol smbl, expr]) = (registerDefine env smbl expr, Nothing)
handleDefine env _ = (registerError env "Bad define", Nothing)

--(List [Symbol "define", Symbol "func", List [Symbol "lambda", List [Symbol "a", Symbol "b" ], List [Symbol "define", Symbol "foo", Symbol "a"], List [Symbol "+", Symbol "foo", Symbol "b"]]])
--(List [List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]], List [Number 1, Number2]]

-- Compute entire AST
computeAST :: Env -> Tree -> (Env, Maybe Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
computeAST env (List list)
    | doesListContainsList list = handleDeepList env list
    | otherwise = handleSimpleList env list
computeAST env tree = handleNoList env tree
