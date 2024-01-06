{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- WatLike
-}

module WatLike
(

) where

import AST

data WatLikeState = WLS [(Int, String)] [(Int, String)] [Instruction] [Instruction]

instance Eq WatLikeState where
    (==) (WLS y z) (WLS y' z') = y == y' && z == z'

instance Show WatLikeState where
    show (WLS y z) = "WLS[< " ++ show y ++ " >< " ++ show z ++ " >]"

getRegisterIndex' :: Int -> String -> [(Int, String)] -> [(Int, String), Int]
getRegisterIndex' maxInd var [] = ([(maxInd, var)], maxInd)
getRegisterIndex' maxInd var (x:xs)
    | var == snd x = ((x:xs), fst x)
    | maxInd > fst x = getRegisterIndex' (maxInd) var xs
    | otherwise = getRegisterIndex' ((fst x) + 1) var xs

getRegisterIndex :: String -> [(Int, String)] -> ([(Int, String)], Int)
getRegisterIndex var indexes = getRegisterIndex' 0 var indexes

getIndex :: String -> [(Int, String)] -> Maybe Int
getIndex var [] = Nothing
getIndex var (x:xs)
    | var == snd x = Just (fst x)
    | otherwise = getIndex var xs

registerAllFuncs :: [FuncDeclaration] -> [(Int, String)] -> [(Int, String)]
registerAllFuncs [] indexes = indexes
registerAllFuncs (((fName, _, _), _):xs) indexes =
    registerAllFuncs xs indexes'
    where
        (indexes', _) = getRegisterIndex fName indexes

registerAllVars :: [Instruction] -> [(Int, String)] -> [(Int, String)]
registerAllVars [] indexes = indexes
registerAllVars ((Declaration ((vName, _), _)):xs) indexes =
    registerAllVars xs indexes'
    where
        (indexes', _) = getRegisterIndex vName indexes

------------------------------------------------------------------------------

-- TODO: take a value, and split in multiple instruction to get the final value
-- return the final value under the form (Var ind) with the variable to where
-- the value will be
-- TODO: use the pattern matching of InstructionToWatLike
ValueToWatLike :: Value -> WatLikeState -> (WatLikeState, [Instruction], Value)
ValueToWatLike 

-- TODO: remove pattern matching for the value part and call ValueToWatLike
InstructionToWatLike :: Instruction -> WatLikeState -> (WatLikeState, [Instruction])
InstructionToWatLike
    (Declaration ((vName, vTyp), FuncValue x))
    (WLS varsIndex funcsIndex oldFunc newFunc) =
InstructionToWatLike
    (Assignation (vName, FuncValue x))
    (WLS varsIndex funcsIndex oldFunc newFunc) =
InstructionToWatLike
    (Function x)
    (WLS varsIndex funcsIndex oldFunc newFunc) =
InstructionToWatLike
    (Return (FuncValue x))
    (WLS varsIndex funcsIndex oldFunc newFunc) =
InstructionToWatLike
    (Cond ())


FuncToWatLike' :: FuncDeclaration -> WatLikeState -> (WatLikeState, FuncDeclaration)
FuncToWatLike' ((fName, fParams, fRet), []) wls = (wls, ((fName, fParams, fRet), []))
FuncToWatLike'  ((fName, fParams, fRet), (x:xs))
                (WLS varsIndex funcsIndex oldFunc newFunc) =

FuncToWatLike :: FuncDeclaration -> WatLikeState -> WatLikeState
FuncToWatLike   ((fName, fParams, fRet), fInss)
                (WLS varsIndex funcsIndex oldFunc newFunc) =
    WLS varsIndex funcsIndex oldFunc newFunc'
    where
        varsIndex' = registerAllVars fInss varsIndex
        (WLS _ _ _ newFunc') =
            FuncToWatLike'
                ((fName, fParams, fRet), fInss)
                (WLS varsIndex' funcsIndex oldFunc newFunc)

------------------------------------------------------------------------------

ASTToWatLike' :: [FuncDeclaration] -> WatLikeState -> WatLikeState
ASTToWatLike' [] (WLS varsIndex funcsIndex oldFunc newFunc) = WLS varsIndex funcsIndex oldFunc newFunc
ASTToWatLike'   (((fName, fParams, fRet), fInss):xs)
                (WLS varsIndex funcsIndex oldFunc newFunc) =
    ASTToWatLike' xs (WLS varsIndex funcsIndex'' oldFunc newFunc')
    where
        (funcsIndex', indFunc) = getRegisterIndex fName funcsIndex
        (WLS _ funcsIndex'' _ newFunc') =
            FuncToWatLike
                ((show indFunc, fParams, fRet), fInss)
                (WLS varsIndex funcsIndex' oldFunc newFunc)

ASTToWatLike :: [FuncDeclaration] -> [FuncDeclaration]
ASTToWatLike funcs = newFunc
    where
        funcsIndex = registerAllFuncs funcs []
        (WLS _ _ _ newFunc) = ASTToWatLike' funcs (WLS [] funcsIndex funcs [])
