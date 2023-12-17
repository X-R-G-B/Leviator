{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module Computing.ComputeAST
    (
        computeAST,
    ) where

import Types
import Computing.ListContainList
import Computing.ReplaceFunctionParams
import Computing.Defines
import Computing.Functions
import Computing.Errors
import Computing.Operators.Calculate
import Computing.Operators.Assert

------------------------- CONDITIONS ---------------------------------

handleIf :: Env -> [Tree] -> (Env, Result)
handleIf env (Boolean (True) : thenBranch : _ : [])
    = computeASTWithoutList env thenBranch
handleIf env (Boolean (False) : _ : elseBranch : [])
    = computeASTWithoutList env elseBranch
handleIf env _ = (registerError env "Bad if statement", Right (undefined))

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
        Nothing -> (registerError env ("Function " ++ smbl ++ " not found"),
            Right (undefined))
        Just func ->
            case computeFunction env func rest of
                (_, Left (Just result)) -> (env, Left (Just result))
                (newEnv, _) -> (env {errors = errors newEnv}, Right(undefined))
handleSimpleList env _ =
    (registerError env "Bad function call", Right (undefined))

-----------------------------------------------------------------------------

handleLambda :: Env -> Tree -> (Env, Result)
handleLambda env (List (List (Symbol "lambda" : List fnParams : fnBodies):
    (List args): _)) =
        computeFunction env
            (Function "" (getParams (List fnParams)) fnBodies) args
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
        (newEnv, Left (Nothing)) ->
            computeFunction newEnv (Function fnName fnParams (xs:rest)) args
        (_, _) ->
            (registerError env "Bad return placement", Right (undefined))
computeFunction env (Function fnName fnParams (x:_)) args =
    case computeFunctionBody env (Function fnName fnParams [x]) args of
        (newEnv, Left (Just replaced)) -> computeAST newEnv replaced
        (newEnv, _) ->
            (registerError newEnv "Missing return in func", Right (undefined))
computeFunction env _ _ =
    (registerError env "Bad function call", Right (undefined))

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


-- Register a define in the Defines list
registerDefine :: Env -> Symbol -> Tree -> Env
registerDefine env symb value@(Number _) = Env (defines env ++ [Define symb value]) (errors env) (functions env)
registerDefine env symb value@(Boolean _) = Env (defines env ++ [Define symb value]) (errors env) (functions env)
registerDefine env symb (List list) = case computeAST env (List list) of
    (_, Left (Just result)) -> Env (defines env ++ [Define symb result]) (errors env) (functions env)
    (newEnv, _) -> registerError newEnv "Bad define"
registerDefine env symb (Symbol smbl) = case getSymbolValue env smbl of
    (_, Just result) -> Env (defines env ++ [Define symb result]) (errors env) (functions env)
    (newEnv, _) -> registerError newEnv "Bad define"

-- Add a function to the Functions list in the Env
addFunction :: Env -> String -> [String] -> [Tree] -> Env
addFunction env fnName fnParams fnBodies
    = Env (defines env) (errors env)
        (functions env ++ [Function fnName fnParams fnBodies])

-- Get params from a function
getParams :: Tree -> [String]
getParams (List []) = []
getParams (List (Symbol smbl : xs)) = smbl : getParams (List xs)
getParams _ = []

-- Register a function in the Functions list
registerFunction :: Env -> Symbol -> Tree -> [Tree] -> Env
registerFunction env "" _ _ =
    registerError env "function name must not be empty"
registerFunction env fnName fnParams fnBodies
    = addFunction env fnName (getParams fnParams) fnBodies

handleDefine :: Env -> Tree -> (Env, Result)
handleDefine env (List [Symbol _, Symbol smbl,
    List (Symbol "lambda": List fnParams : fnBodies)]) =
        (registerFunction env smbl (List fnParams) fnBodies, Left (Nothing))
handleDefine env (List [Symbol _,
    (List (Symbol smbl : fnParams)), List fnBodies]) =
        (registerFunction env smbl (List fnParams) (List fnBodies : []),
            Left (Nothing))
handleDefine env (List [Symbol _, Symbol smbl, expr]) =
    (registerDefine env smbl expr, Left (Nothing))
handleDefine env _ = (registerError env "Bad define", Right (undefined))








computeAST :: Env -> Tree -> (Env, Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
computeAST env tree@(List (List (Symbol "lambda" : _) : _)) =
    handleLambda env tree
computeAST env tree@(List _) = computeAstWithList env tree
computeAST env tree = computeASTWithoutList env tree
