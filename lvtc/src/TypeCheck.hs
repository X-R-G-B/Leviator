{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Type checker
-}

module TypeCheck
    ( typeCheck
    ) where

import AST
import Debug.Trace

data Env = Env FuncPrototype [VarAssignation] [FuncPrototype]

findVarInParams :: Symbol -> [Var] -> Env -> Maybe Type
findVarInParams _ [] _ = Nothing
findVarInParams s ((s', t):xs) env
  | s == s' = Just t
  | otherwise = findVarInParams s xs env

findVarInVar :: Symbol -> [VarAssignation] -> Env -> Maybe Type
findVarInVar _ [] _ = Nothing
findVarInVar s ((s', v):xs) env
  | s == s' = findValueType v env
  | otherwise = findVarInVar s xs env

findTypeReturnFunc :: Symbol -> [FuncPrototype] -> Maybe FuncPrototype
findTypeReturnFunc _ [] = Nothing
findTypeReturnFunc s ((s', ps, t):xs)
  | s == s' = Just (s', ps, t)
  | otherwise = findTypeReturnFunc s xs

getValueType :: Value -> Maybe Type
getValueType (Integer _) = Just "Int"
getValueType (Boolean _) = Just "Bool"
getValueType (StringView _) = Just "String"
getValueType _ = Nothing

checkFuncType :: [Value] -> Env -> Maybe FuncPrototype -> Maybe Type
checkFuncType _ _ Nothing = Nothing
checkFuncType [] _ _ = Nothing
checkFuncType callParams (Env proto params env) (Just (_, ps, t))
  | checkVarsTypes callParams (Just ps) (Env proto params env) = Just t
  | otherwise = Nothing

handleFinders :: Maybe Type -> Symbol -> Env -> Maybe Type
handleFinders _ _ (Env (_, [], _) [] _) = Nothing
handleFinders Nothing s (Env proto vars env) = findVarInVar s vars (Env proto vars env)
handleFinders (Just t) _ _ = Just t

findValueType :: Value -> Env -> Maybe Type
findValueType (Var s) (Env (ps, params, t) vars env) =
  handleFinders (findVarInParams s params (Env (ps, params, t) vars env)) s (Env (ps, params, t) vars env)
findValueType (FuncValue (s, vs)) (Env proto vars env) = checkFuncType vs (Env proto vars env) (findTypeReturnFunc s env)
findValueType v _ = getValueType v

checkValueType :: Type -> Maybe Type -> Bool
checkValueType _ Nothing = False
checkValueType t (Just t') = t == t'

assertTypeAndValue :: Type -> Value -> Env -> Bool
assertTypeAndValue _ (Var _) (Env (_, [], _) [] _) = False
assertTypeAndValue _ (FuncValue _) (Env (_, _, _) _ []) = False
assertTypeAndValue t v env = checkValueType t (findValueType v env)

assertMTypeAndValue :: Maybe Type -> Value -> Env -> Bool
assertMTypeAndValue Nothing _ _ = False
assertMTypeAndValue (Just t) v env = assertTypeAndValue t v env

isTypeValid :: Type -> Bool
isTypeValid "Int" = True
isTypeValid "Bool" = True
isTypeValid _ = False

checkVarsTypes :: [Value] -> Maybe [Var] -> Env -> Bool
checkVarsTypes [] _ _ = False
checkVarsTypes _ Nothing _ = False
checkVarsTypes values (Just param) _ | length values /= length param = False
checkVarsTypes [v] (Just [(_, t)]) env = assertTypeAndValue t v env
checkVarsTypes (v:vs) (Just ((_, t):xs)) env
  | assertTypeAndValue t v env = checkVarsTypes vs (Just xs) env
  | otherwise = False
checkVarsTypes _ _ _ = False

findFunc :: Symbol -> [FuncPrototype] -> Maybe [Var]
findFunc _ [] = Nothing
findFunc s ((s', params, _):_) | s == s' = Just params
findFunc s (_:xs) = findFunc s xs

checkCall :: FuncCall -> Env -> [Instruction] -> Bool
checkCall (symbol, values) (Env (s, params, t) vars env) xs
  | s == symbol && checkVarsTypes values (Just params) (Env (s, params, t) vars env) ||
    checkVarsTypes values (findFunc symbol env) (Env (s, params, t) vars env) =
    checkInstructions xs (Env (s, params, t) vars env)
  | otherwise = False

checkReturn :: Value -> Env -> [Instruction] -> Bool
checkReturn v (Env (s, params, t) vars env) xs
  | assertTypeAndValue t v (Env (s, params, t) vars env) =
    checkInstructions xs (Env (s, params, t) vars env)
checkReturn _ _ _ = False

checkDeclaration :: VarDeclaration -> Env -> [Instruction] -> Bool
checkDeclaration ((s, t), v) (Env proto vars env) xs |
  assertTypeAndValue t v (Env proto vars env) =
  checkInstructions xs (Env proto ((s, v):vars) env)
checkDeclaration _ _ _ = False

checkAssignation :: VarAssignation -> Env -> [Instruction] -> Bool
checkAssignation (s, v) env xs | assertMTypeAndValue (findValueType (Var s) env) v env =
  checkInstructions xs env
checkAssignation _ _ _ = False

checkCondition :: Condition -> Env -> [Instruction] -> Bool
checkCondition (v, fst_, snd_) env xs
  | assertTypeAndValue "Bool" v env && checkInstructions fst_ env &&
    checkInstructions snd_ env = checkInstructions xs env
checkCondition _ _ _ = False

checkInstructions :: [Instruction] -> Env -> Bool
checkInstructions [] _ = True
checkInstructions ((Call func):xs) env = checkCall func env xs
checkInstructions ((Return ret):xs) env = checkReturn ret env xs
checkInstructions ((Declaration declaration):xs) env = checkDeclaration declaration env xs
checkInstructions ((Assignation assignation):xs) env = checkAssignation assignation env xs
checkInstructions ((Cond condition):xs) env = checkCondition condition env xs

checkVarTypes :: [Var] -> Bool
checkVarTypes [] = True
checkVarTypes [x] = isTypeValid (snd x)
checkVarTypes (x:xs) | isTypeValid (snd x) = checkVarTypes xs

checkFunction :: FuncDeclaration -> [FuncPrototype] -> Bool
checkFunction ((_, args, _), _) _ | not (checkVarTypes args) = False
checkFunction ((_, _, t), _) _ | not (isTypeValid t) = False
checkFunction (prototype, instructions) env
  | checkInstructions instructions (Env prototype [] env) = True
checkFunction _ _ = False

checkNotExisting :: FuncDeclaration -> [FuncPrototype] -> Bool
checkNotExisting _ [] = True
checkNotExisting ((s, args, t), instr) ((ls, _, _):xs)
    | s == ls = False
    | otherwise = checkNotExisting ((s, args, t), instr) xs

receiveCheckFuncRes :: Bool -> [FuncDeclaration] -> FuncDeclaration -> [FuncPrototype] -> Bool
receiveCheckFuncRes True xs (prototype, _) env = checkDeclarations xs (Just (prototype:env))
receiveCheckFuncRes _ _ _ _ = False

checkDeclarations :: [FuncDeclaration] -> Maybe [FuncPrototype] -> Bool
checkDeclarations _ Nothing = False
checkDeclarations [] (Just _) = True
checkDeclarations (func:xs) (Just env)
  | checkNotExisting func env =
   receiveCheckFuncRes (checkFunction func env) xs func env
  | otherwise =  False

createCalcPrototype :: Symbol -> Type -> FuncPrototype
createCalcPrototype s t = (s, [("x", t), ("y", t)], "Int")

createCompPrototype :: Symbol -> Type -> FuncPrototype
createCompPrototype s t = (s, [("x", t), ("y", t)], "Bool")

createCompPolyMorph :: Symbol -> [Type] -> [FuncPrototype]
createCompPolyMorph _ [] = []
createCompPolyMorph s (x:xs) = createCompPrototype s x : createCompPolyMorph s xs

createCompOp :: [Symbol] -> [FuncPrototype]
createCompOp [] = []
createCompOp (x:xs) = createCompPolyMorph x ["Int", "Bool"] ++ createCompOp xs

createCalcOp :: [Symbol] -> [FuncPrototype]
createCalcOp [] = []
createCalcOp (x:xs) = createCalcPrototype x "Int" : createCalcOp xs

defaultEnv :: Maybe [FuncPrototype]
defaultEnv = Just (createCalcOp ["+", "-", "*", "%", "/"] ++ createCompOp ["==", "!=", "<", ">", "<=", ">="])

typeCheck :: [FuncDeclaration] -> Bool
typeCheck expressions = checkDeclarations expressions defaultEnv
