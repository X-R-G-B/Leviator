{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- WatLike
-}

module WatLike
(
    FuncDeclare
    , Index
    , aSTToWatLike
) where

import AST
import Builtins

import Data.Int (Int32)
import Data.Char (ord)

------------------------------------------------------------------------------
-- Named Index Vars / Func

type Index = (Int, String)
type FuncDeclare = (FuncDeclaration, [Index])

getRegisterIndex' :: Int -> String -> [Index] -> ([Index], Int)
getRegisterIndex' maxInd var [] = ([(maxInd, var)], maxInd)
getRegisterIndex' maxInd var (x:xs)
    | var == snd x = (x:xs, fst x)
    | maxInd > fst x = (x : inds, ind)
    | otherwise = (x : inds', ind')
    where
        (inds, ind) = getRegisterIndex' maxInd var xs
        (inds', ind') = getRegisterIndex' (fst x + 1) var xs

getRegisterIndex :: String -> [Index] -> ([Index], Int)
getRegisterIndex = getRegisterIndex' 0

newIndex' :: Int -> [Index] -> ([Index], Int)
newIndex' maxInd [] = ([(maxInd, "_tmpValue")], maxInd)
newIndex' maxInd (x:xs)
    | maxInd > fst x = (x : inds, ind)
    | otherwise = (x : inds', ind')
    where
        (inds, ind) = newIndex' maxInd xs
        (inds', ind') = newIndex' (fst x + 1) xs

newIndex :: [Index] -> ([Index], Int)
newIndex = newIndex' 0

modifyAllValue :: [Value] -> [Index] -> [Index] -> ([Value], [Index], [Index])
modifyAllValue [] varsIndex funcsIndex = ([], varsIndex, funcsIndex)
modifyAllValue (x:xs) varsIndex funcsIndex =
    (val:vals, varsIndex'', funcsIndex'')
    where
        (val, varsIndex', funcsIndex') = modifyAll' x varsIndex funcsIndex
        (vals, varsIndex'', funcsIndex'') =
            modifyAllValue xs varsIndex' funcsIndex'

modifyAll' :: Value -> [Index] -> [Index] -> (Value, [Index], [Index])
modifyAll' (FuncValue (fName, vals)) varsIndex funcsIndex =
    (newFunc, varsIndex'', funcsIndex'')
    where
        (funcsIndex', indFunc) = getRegisterIndex fName funcsIndex
        (vals', varsIndex'', funcsIndex'') =
            modifyAllValue vals varsIndex funcsIndex'
        newFunc = FuncValue (show indFunc, vals')
modifyAll' (Var vName) varsIndex funcsIndex = (newVar, varsIndex', funcsIndex)
    where
        (varsIndex', indVar) = getRegisterIndex vName varsIndex
        newVar = Var (show indVar)
modifyAll' x varsIndex funcsIndex = (x, varsIndex, funcsIndex)

---

modifyAll :: [Instruction] -> [Index] -> [Index]
            -> ([Instruction], [Index], [Index])
modifyAll [] varsIndex funcsIndex = ([], varsIndex, funcsIndex)
modifyAll ((Function (fName, vals)):xs) varsIndex funcsIndex =
    (newFunc:ins', varsIndex''', funcsIndex''')
    where
        (funcsIndex', indFunc) = getRegisterIndex fName funcsIndex
        (vals', varsIndex'', funcsIndex'') =
            modifyAllValue vals varsIndex funcsIndex'
        newFunc = Function (show indFunc, vals')
        (ins', varsIndex''', funcsIndex''') =
            modifyAll xs varsIndex'' funcsIndex''
modifyAll ((Return vValue):xs) varsIndex funcsIndex =
    (newReturn:ins', varsIndex'', funcsIndex'')
    where
        (vValue', varsIndex', funcsIndex') =
            modifyAll' vValue varsIndex funcsIndex
        newReturn = Return vValue'
        (ins', varsIndex'', funcsIndex'') = modifyAll xs varsIndex' funcsIndex'
modifyAll ((Declaration ((vName, vTyp), vValue)):xs) varsIndex funcsIndex =
    (newDeclaration : ins', varsIndex'', funcsIndex'')
    where
        (varsIndex', ind) = getRegisterIndex vName varsIndex
        newDeclaration = Declaration ((show ind, vTyp), vValue)
        (ins', varsIndex'', funcsIndex'') = modifyAll xs varsIndex' funcsIndex
modifyAll ((Assignation (vName, vValue)):xs) varsIndex funcsIndex =
    (newAssignation:ins', varsIndex''', funcsIndex''')
    where
        (varsIndex', ind) = getRegisterIndex vName varsIndex
        (vValue', varsIndex'', funcsIndex'') =
            modifyAll' vValue varsIndex' funcsIndex
        newAssignation = Assignation (show ind, vValue')
        (ins', varsIndex''', funcsIndex''') =
            modifyAll xs varsIndex'' funcsIndex''
modifyAll ((Cond (vValue, insIf, insElse)):xs) vsInd fsInd =
    (newCond:ins', vsInd'''', fsInd'''')
    where
        (vValue', vsInd', fsInd') = modifyAll' vValue vsInd fsInd
        (insIf', vsInd'', fsInd'') = modifyAll insIf vsInd' fsInd'
        (insElse', vsInd''', fsInd''') = modifyAll insElse vsInd'' fsInd''
        newCond = Cond (vValue', insIf', insElse')
        (ins', vsInd'''', fsInd'''') = modifyAll xs vsInd''' fsInd'''

transformType :: Type -> Type
transformType "Void" = "Int"
transformType "Char" = "Int"
transformType "Bool" = "Int"
transformType x = x

registerParams :: FuncDeclare -> FuncDeclare
registerParams (((fName, [], typ), ins), varsIndex) =
    (((fName, [], transformType typ), ins), varsIndex)
registerParams (((fName, (pName, pTyp):vParams, typ), ins), varsIndex) =
    (((fName', newParams:vParams', vTyp'), ins), varsIndex'')
    where
        (varsIndex', indVar) = getRegisterIndex pName varsIndex
        (((fName', vParams', vTyp'), _), varsIndex'') =
            registerParams (((fName, vParams, typ), ins), varsIndex')
        newParams = (show indVar, transformType pTyp)

registerAllFuncs :: [FuncDeclaration] -> [Index] -> [Index]
registerAllFuncs [] funcsIndex = funcsIndex
registerAllFuncs (((fName, _, _), _):xs) funcsIndex = funcsIndex''
    where
        (funcsIndex', _) = getRegisterIndex fName funcsIndex
        funcsIndex'' = registerAllFuncs xs funcsIndex'

changeIndexes :: [FuncDeclaration] -> [Index] -> ([FuncDeclare], [Index])
changeIndexes [] funcsIndex = ([], funcsIndex)
changeIndexes (((fName, vars, typ), ins):xs) funcsIndex =
    (newFunc:funcs, funcsIndex''')
    where
        (funcsIndex', indFunc) = getRegisterIndex fName funcsIndex
        (((_, vars', typ'), ins'), varsIndex) =
            registerParams (((fName, vars, typ), ins), [])
        (ins'', varsIndex'', funcsIndex'') =
            modifyAll ins' varsIndex funcsIndex'
        newFunc = (((show indFunc, vars', typ'), ins''), varsIndex'')
        (funcs, funcsIndex''') = changeIndexes xs funcsIndex''

------------------------------------------------------------------------------

data WatLikeState = WLS [Index] [FuncDeclare] [FuncDeclare]

instance Eq WatLikeState where
    (==) (WLS x y z) (WLS x' y' z') = x == x' && y == y' && z == z'

instance Show WatLikeState where
    show (WLS x y z) =
        "WLS[[ " ++ show x ++ " ][ " ++ show y ++ " ][ " ++ show z ++ " ]]"

------------------------------------------------------------------------------

getPrototype :: String -> [FuncDeclare] -> FuncPrototype
getPrototype _ [] = undefined
getPrototype fName ((((fName', vars, typ), _), _):xs)
    | fName == fName' = (fName', vars, typ)
    | otherwise = getPrototype fName xs

------------------------------------------------------------------------------

funcCallToWatLike :: FuncCall -> ([FuncDeclare], [Index]) -> [Index]
                    -> ([Index], [Instruction], FuncCall)
funcCallToWatLike (fName, []) _ varsIndex = (varsIndex, [], (fName, []))
funcCallToWatLike (fName, vVal:vVals) oldFuncs varsIndex =
    (varsIndex'', ins ++ inss, (fName, vVal':vVals'))
    where
        (varsIndex', ins, vVal') = valueToWatLike vVal oldFuncs varsIndex
        (varsIndex'', inss, (_, vVals')) =
            funcCallToWatLike (fName, vVals) oldFuncs varsIndex'

valueToWatLike :: Value -> ([FuncDeclare], [Index]) -> [Index]
                -> ([Index], [Instruction], Value)
valueToWatLike (FuncValue x) (oldFuncs, funcsIndex) varsIndex =
    (varsIndex'', ins ++ [newDeclaration], Var (show indVar))
    where
        (varsIndex', ins, (fName, vVals)) =
            funcCallToWatLike x (oldFuncs, funcsIndex) varsIndex
        (varsIndex'', indVar) = newIndex varsIndex'
        (_, _, typ) = getPrototype fName oldFuncs
        newDeclaration =
            Declaration ((show indVar, typ), FuncValue (fName, vVals))
valueToWatLike (Boolean True) _ varsIndex =
    (varsIndex', [newDeclaration], Var (show indVar))
    where
        (varsIndex', indVar) = newIndex varsIndex
        newDeclaration = Declaration ((show indVar, "Int"), Integer 1)
valueToWatLike (Boolean False) _ varsIndex =
    (varsIndex', [newDeclaration], Var (show indVar))
    where
        (varsIndex', indVar) = newIndex varsIndex
        newDeclaration = Declaration ((show indVar, "Int"), Integer 0)
valueToWatLike (Character x) _ varsIndex =
    (varsIndex', [newDeclaration], Var (show indVar))
    where
        (varsIndex', indVar) = newIndex varsIndex
        ordChar = read (show (ord x)) :: Int32
        newDeclaration = Declaration ((show indVar, "Int"), Integer ordChar)
valueToWatLike (StringView _) _ _ = undefined
valueToWatLike Void _ varsIndex =
    (varsIndex', [newDeclaration], Var (show indVar))
    where
        (varsIndex', indVar) = newIndex varsIndex
        newDeclaration = Declaration ((show indVar, "Int"), Integer 0)
valueToWatLike (Integer x) _ varsIndex =
    (varsIndex', [newDeclaration], Var (show indVar))
    where
        (varsIndex', indVar) = newIndex varsIndex
        newDeclaration = Declaration ((show indVar, "Int"), Integer x)
valueToWatLike (Var x) _ varsIndex = (varsIndex, [], Var x)

instructionToWatLike :: Instruction -> ([FuncDeclare], [Index]) -> [Index]
                    -> ([Index], [Instruction])
instructionToWatLike
    (Declaration ((vName, vTyp), vValue)) oldFuncs varsIndex =
    (varsIndex', ins' ++ [newDeclaration])
    where
        (varsIndex', ins', vValue') = valueToWatLike vValue oldFuncs varsIndex
        newDeclaration = Declaration ((vName, vTyp), vValue')
instructionToWatLike
    (Assignation (vName, vValue)) oldFuncs varsIndex =
    (varsIndex', ins' ++ [newAssignation])
    where
        (varsIndex', ins', vValue') = valueToWatLike vValue oldFuncs varsIndex
        newAssignation = Assignation (vName, vValue')
instructionToWatLike
    (Function (fName, fParams)) oldFuncs varsIndex =
    (varsIndex', ins' ++ [newFunction])
    where
        (varsIndex', ins', (_, fParams')) =
            funcCallToWatLike (fName, fParams) oldFuncs varsIndex
        newFunction = Function (fName, fParams')
instructionToWatLike
    (Return vValue) oldFuncs varsIndex =
    (varsIndex', ins' ++ [newReturn])
    where
        (varsIndex', ins', vValue') = valueToWatLike vValue oldFuncs varsIndex
        newReturn = Return vValue'
instructionToWatLike
    (Cond (vValCond, vInsTrue, vInsFalse)) oldFuncs vsInd =
    (vsInd''', insCond ++ [newCond])
    where
        (vsInd', insCond, vValCond') = valueToWatLike vValCond oldFuncs vsInd
        (vsInd'', vInsTrue') = instructionsToWatLike vInsTrue oldFuncs vsInd'
        (vsInd''', vInsFalse') =
            instructionsToWatLike vInsFalse oldFuncs vsInd''
        newCond = Cond (vValCond', vInsTrue', vInsFalse')

instructionsToWatLike :: [Instruction] -> ([FuncDeclare], [Index])
                    -> [Index] -> ([Index], [Instruction])
instructionsToWatLike [] _ varsIndex = (varsIndex, [])
instructionsToWatLike (x:xs) oldFuncs varsIndex =
    (varsIndex'', ins ++ inss)
    where
        (varsIndex', ins) = instructionToWatLike x oldFuncs varsIndex
        (varsIndex'', inss) = instructionsToWatLike xs oldFuncs varsIndex'

------------------------------------------------------------------------------

funcToWatLike' :: FuncDeclare -> ([FuncDeclare], [Index]) -> FuncDeclare
funcToWatLike' (((fName, fParams, fRet), []), varsIndex) _ =
    (((fName, fParams, fRet), []), varsIndex)
funcToWatLike'  (((fName, fParams, fRet), ins:inss), varsIndex) oldFuncs =
    (((fName, fParams, fRet), ins' ++ inss'), varsIndex'')
    where
        (varsIndex', ins') = instructionToWatLike ins oldFuncs varsIndex
        thisFunc = (((fName, fParams, fRet), inss), varsIndex')
        (((_, _, _), inss'), varsIndex'') = funcToWatLike' thisFunc oldFuncs

funcToWatLike :: FuncDeclare -> WatLikeState -> WatLikeState
funcToWatLike   (((fName, fParams, fRet), fInss), varsIndex)
                (WLS funcsIndex oldFuncs newFunc) =
    WLS funcsIndex oldFuncs (newFunc ++ [fFunc])
    where
        fFunc = funcToWatLike'
            (((fName, fParams, fRet), fInss), varsIndex)
            (oldFuncs, funcsIndex)

------------------------------------------------------------------------------

aSTToWatLike' :: [FuncDeclare] -> WatLikeState -> WatLikeState
aSTToWatLike' [] (WLS funcsIndex oldFunc newFunc) =
    WLS funcsIndex oldFunc newFunc
aSTToWatLike'   (func:xs)
                (WLS funcsIndex oldFunc newFunc) =
    aSTToWatLike' xs (WLS funcsIndex' oldFunc newFunc')
    where
        (WLS funcsIndex' _ newFunc') =
            funcToWatLike
                func
                (WLS funcsIndex oldFunc newFunc)

aSTToWatLike :: [FuncDeclaration] -> [FuncDeclare]
aSTToWatLike funcs = newFunc
    where
        allFuncs = getBuiltinsFunc ++ funcs
        funcsIndex = registerAllFuncs allFuncs []
        (funcs', funcsIndex') = changeIndexes allFuncs funcsIndex
        (WLS _ _ newFunc) = aSTToWatLike' funcs' (WLS funcsIndex' funcs' [])
