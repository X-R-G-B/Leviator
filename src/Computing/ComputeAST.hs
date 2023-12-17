{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module Computing.ComputeAST
    (
        computeAST,
        evaluateSymbol
    ) where

import Types
import Computing.ListContainList
import Computing.ReplaceFunctionParams
import Computing.Defines
import Computing.Functions
import Computing.Errors

-- Compute a "+ - div * mod" list, using defines if needed

addition :: Env -> [Tree] -> (Env, Result)
addition env [Number a, Number b] = (env, Left (Just (Number (a + b))))
addition env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Number (a + symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
addition env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Number (symbolValue + b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
addition env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Number (symbolValueA + symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
addition env list
    | length list /= 2 = (registerError env "Addition need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in addition", Right (undefined))

multiplication :: Env -> [Tree] -> (Env, Result)
multiplication env [Number a, Number b] = (env, Left (Just (Number (a * b))))
multiplication env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left(Just (Number (a * symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
multiplication env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Number (symbolValue * b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
multiplication env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Number (symbolValueA * symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
multiplication env list
    | length list /= 2 = (registerError env "* need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in multiplication", Right (undefined))

subtraction :: Env -> [Tree] -> (Env, Result)
subtraction env [Number a, Number b] = (env, Left (Just (Number (a - b))))
subtraction env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Number (a - symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
subtraction env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Number (symbolValue - b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
subtraction env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Number (symbolValueA - symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
subtraction env list
    | length list /= 2 = (registerError env "- need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in subtraction", Right (undefined))

division :: Env -> [Tree] -> (Env, Result)
division env [Number a, Number b]
    | b == 0 = (registerError env "Division by 0", Right (undefined))
    | otherwise = (env, Left (Just (Number (a `div` b))))
division env [Symbol a, Number b]
    | b == 0 = (registerError env "Division by 0", Right (undefined))
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Number (symbolValue `div` b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
division env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b
    , symbolValue == 0 = (registerError env "Division by 0", Right (undefined))
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Number (a `div` symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
division env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b
    , symbolValueB == 0 = (registerError env "Division by 0", Right (undefined))
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Number (symbolValueA `div` symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
division env list
    | length list /= 2 = (registerError env "/ need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in division", Right (undefined))

modulo :: Env -> [Tree] -> (Env, Result)
modulo env [Number a, Number b]
    | b == 0 = (registerError env "Modulo by 0", Right (undefined))
    | otherwise = (env, Left (Just (Number (a `mod` b))))
modulo env [Symbol a, Number b]
    | b == 0 = (registerError env "Modulo by 0", Right (undefined))
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Number (symbolValue `mod` b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
modulo env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b
    , symbolValue == 0 = (registerError env "Modulo by 0", Right (undefined))
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Number (a `mod` symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
modulo env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b
    , symbolValueB == 0 = (registerError env "Modulo by 0", Right (undefined))
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Number (symbolValueA `mod` symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
modulo env list
    | length list /= 2 = (registerError env "% need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in modulo", Right (undefined))

------------------------- CONDITIONS ---------------------------------

equal :: Env -> [Tree] -> (Env, Result)
equal env [Number a, Number b] = (env, Left (Just (Boolean (a == b))))
equal env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (a == symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
equal env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Boolean (symbolValue == b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
equal env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (symbolValueA == symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
equal env list
    | length list /= 2 = (registerError env "eq? need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in eq?", Right (undefined))

notEqual :: Env -> [Tree] -> (Env, Result)
notEqual env [Number a, Number b] = (env, Left (Just (Boolean (a /= b))))
notEqual env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (a /= symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
notEqual env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Boolean (symbolValue /= b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
notEqual env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (symbolValueA /= symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
notEqual env list
    | length list /= 2 = (registerError env "not-eq? need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in not-eq?", Right (undefined))

inferior :: Env -> [Tree] -> (Env, Result)
inferior env [Number a, Number b] = (env, Left (Just (Boolean (a < b))))
inferior env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (a < symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
inferior env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Boolean (symbolValue < b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
inferior env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (symbolValueA < symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
inferior env list
    | length list /= 2 = (registerError env "< need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in <", Right (undefined))

superior :: Env -> [Tree] -> (Env, Result)
superior env [Number a, Number b] = (env, Left (Just (Boolean (a > b))))
superior env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (a > symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
superior env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Boolean (symbolValue > b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
superior env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (symbolValueA > symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
superior env list
    | length list /= 2 = (registerError env "> need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in >", Right (undefined))

inferiorOrEqual :: Env -> [Tree] -> (Env, Result)
inferiorOrEqual env [Number a, Number b] = (env, Left (Just (Boolean (a <= b))))
inferiorOrEqual env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (a <= symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
inferiorOrEqual env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Boolean (symbolValue <= b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
inferiorOrEqual env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (symbolValueA <= symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
inferiorOrEqual env list
    | length list /= 2 = (registerError env "<= need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in <=", Right (undefined))

superiorOrEqual :: Env -> [Tree] -> (Env, Result)
superiorOrEqual env [Number a, Number b] = (env, Left (Just (Boolean (a >= b))))
superiorOrEqual env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (a >= symbolValue))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
superiorOrEqual env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- evaluateSymbol env a =
        (env, Left (Just (Boolean (symbolValue >= b))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
superiorOrEqual env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- evaluateSymbol env a
    , (_, Just (Number symbolValueB)) <- evaluateSymbol env b =
        (env, Left (Just (Boolean (symbolValueA >= symbolValueB))))
    | otherwise = (registerError env "Symbol not found", Right (undefined))
superiorOrEqual env list
    | length list /= 2 = (registerError env ">= need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in >=", Right (undefined))

handleIf :: Env -> [Tree] -> (Env, Result)
handleIf env (Boolean (True) : thenBranch : _ : [])
    = computeASTWithoutList env thenBranch
handleIf env (Boolean (False) : _ : elseBranch : [])
    = computeASTWithoutList env elseBranch
handleIf env _ = (registerError env "Bad if statement", Right (undefined))

----------------------------------------------------------------------------------

-- Evaluate a symbol and return its value
evaluateSymbol :: Env -> Symbol -> (Env, Maybe Tree)
evaluateSymbol env smbl =
    case getSymbolValue env smbl of
        (_, Nothing) -> (env, Nothing)
        (_, Just (Number number)) -> (env, Just (Number number))
        (_, Just (Boolean value)) -> (env, Just (Boolean value))
        (_, Just (List list)) ->
            case computeAST env (List list) of
                (_, Left (Just result)) -> (env, Just result)
                (_, _) -> (env, Nothing)
        (_, _) -> (env, Nothing)

----------------------------------------------------------------------------------


-- Find nested lists and resolve them
resolveNestedLists :: Env -> [Tree] -> [Tree] -> (Env, Maybe [Tree])
resolveNestedLists env resolvedList [] = (env, Just resolvedList)
resolveNestedLists env resolvedList (List list : rest)
    | not (doesListContainsList list) =
        case handleSimpleList env list of
            (newEnv, Left (Just resolved)) ->
                resolveNestedLists newEnv (resolvedList ++ [resolved]) rest
            (newEnv, _) -> (newEnv, Nothing)
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
handleSimpleList :: Env -> [Tree] -> (Env, Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "div" : rest) = division env rest
handleSimpleList env (Symbol "mod" : rest) = modulo env rest
handleSimpleList env (Symbol "eq?" : rest) = equal env rest
handleSimpleList env (Symbol "diff?" : rest) = notEqual env rest
handleSimpleList env (Symbol "<" : rest) = inferior env rest
handleSimpleList env (Symbol ">" : rest) = superior env rest
handleSimpleList env (Symbol "<=" : rest) = inferiorOrEqual env rest
handleSimpleList env (Symbol ">=" : rest) = superiorOrEqual env rest
handleSimpleList env (Symbol "if" : rest) = handleIf env rest
handleSimpleList env (Symbol smbl : rest) =
    case getFunctionByName env smbl of
        Nothing -> (registerError env ("Function " ++ smbl ++ " not found"), Right (undefined))
        Just func ->
            case computeFunction env func rest of
                (_, Left (Just result)) -> (env, Left (Just result))
                (newEnv, _) -> (env { errors = errors newEnv }, Right (undefined))
handleSimpleList env _ = (registerError env "Bad function call", Right (undefined))

----------------------------------------------------------------------------------

handleLambda :: Env -> Tree -> (Env, Result)
handleLambda env (List (List (Symbol "lambda" : List fnParams : fnBodies): (List args): _))
    = computeFunction env (Function "" (getParams (List fnParams)) fnBodies) args
handleLambda env _ = (registerError env "Bad lambda", Left (Nothing))

--------------------------- COMPUTE FUNCTIONS --------------------------------

computeFunctionBody :: Env -> Function -> [Tree] -> (Env, Result)
computeFunctionBody env (Function _ _ []) _ = (env, Left (Nothing))
computeFunctionBody env (Function _ fnParams (x:_)) args =
    case replaceFunctionParams env fnParams x args of
        (newEnv, Nothing) -> (newEnv, Right (undefined))
        (newEnv, Just replaced) -> computeAST newEnv replaced

computeFunction :: Env -> Function -> [Tree] -> (Env, Result)
computeFunction env (Function fnName fnParams (x:xs:rest)) args =
    case computeFunctionBody env (Function fnName fnParams [x]) args of
        (newEnv, Left (Nothing)) -> computeFunction newEnv (Function fnName fnParams (xs:rest)) args
        (_, _) -> (registerError env "Return needs to be the last statement", Right (undefined))
computeFunction env (Function fnName fnParams (x:_)) args =
    case computeFunctionBody env (Function fnName fnParams [x]) args of
        (newEnv, Left (Just replaced)) -> computeAST newEnv replaced
        (newEnv, _) -> (registerError newEnv "Missing return in function", Right (undefined))
computeFunction env _ _ = (registerError env "Bad function call", Right (undefined))


--------------------------- COMPUTE AST -------------------------------------

computeASTWithoutList :: Env -> Tree -> (Env, Result)
computeASTWithoutList env (Number nbr) = (env, Left (Just (Number nbr)))
computeASTWithoutList env (Boolean value) = (env, Left (Just (Boolean value)))
computeASTWithoutList env (Symbol smbl)
    | Nothing <- value = (env, Right (undefined))
    | Just (List list) <- value = computeAST env (List list)
    | Just result <- value = (env, Left (Just result))
        where (_, value) = getSymbolValue env smbl
computeASTWithoutList env _ = (env, Right (undefined))

computeAstWithList :: Env -> Tree -> (Env, Result)
computeAstWithList env (List list)
    | not (doesListContainsList list) = handleSimpleList env list
    | otherwise = case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Right (undefined))
            (newEnv, Just rvd) -> computeAST newEnv (List rvd)
computeAstWithList env _ = (registerError env "Bad list", Right (undefined))

computeAST :: Env -> Tree -> (Env, Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
computeAST env tree@(List (List (Symbol "lambda" : _) : _)) = handleLambda env tree
computeAST env tree@(List _) = computeAstWithList env tree
computeAST env tree = computeASTWithoutList env tree
