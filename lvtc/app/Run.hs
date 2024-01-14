{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Run
-}

module Run
(
    run
) where

import Expression (parseAllExpression, Expression (..))
import Alias (proceedAlias)
import ParseLvt (parseFuncDeclaration)
import WatLike (aSTToWatLike, FuncDeclare)
import Parser (runParser)
import AST (FuncDeclaration)
import WatAST (FuncDef)
import WatLikeToWat (watsLikeToWat)
import Wasm (Wasm)
import WatToWasm (watToWasm)
import WriteWasm (writeWasm)
import TypeCheck (typeCheck)
import Args

import System.Directory (listDirectory)
import System.FilePath (joinPath)
import Data.List (isSuffixOf)

getExpressionFromFile :: FilePath -> IO [Expression]
getExpressionFromFile path =
    readFile path
    >>= (\str ->
        case runParser (proceedAlias <$> parseAllExpression) str of
            Nothing -> fail ("Invalid expression found in file: " ++ show path)
            Just (expression, _) -> return expression)

getFilesExpression :: Bool -> [FilePath] -> IO [Expression]
getFilesExpression v (file:files) =
    p v
    >> getExpressionFromFile file
        >>= (\expFile -> getFilesExpression v files
            >>= (\expFiles -> return (expFile ++ expFiles)))
    where
        p True = putStrLn ("Parsing expressions from: " ++ show file ++ "...")
        p False = return ()
getFilesExpression _ [] = return []

selectGoodFiles :: Bool -> FilePath -> [FilePath] -> IO [FilePath]
selectGoodFiles _ _ [] = return []
selectGoodFiles v folder (file:files)
    | ".lvt" `isSuffixOf` trueFile =
            p v
            >> selectGoodFiles v folder files
                >>= (\others -> return (trueFile : others))
    | otherwise = selectGoodFiles v folder files
    where
        trueFile = joinPath [folder, file]
        p True = putStrLn ("- " ++ show trueFile)
        p False = return ()

listAllFiles :: Bool -> FilePath -> IO [FilePath]
listAllFiles v path =
    p v
    >> listDirectory path >>= selectGoodFiles v path
    where
        p True = putStrLn ("Compiling Folder: " ++ show path)
        p False = return ()

listsAllFiles :: Bool -> [FilePath] -> IO [FilePath]
listsAllFiles _ [] = return []
listsAllFiles v (f:fs) =
    listAllFiles v f
        >>= (\files -> listsAllFiles v fs
            >>= (\others -> return (files ++ others)))

getAllFunc :: Bool -> [Expression] -> IO [FuncDeclaration]
getAllFunc _ [] = return []
getAllFunc v ((Expression.Function str):expressions) =
    case runParser parseFuncDeclaration str of
        Nothing -> fail ("Parser Error: " ++ show str)
        Just (func, _) ->
            getAllFunc v expressions >>= \funcs -> return (func:funcs)
getAllFunc v (x : expressions) = p v >> getAllFunc v expressions
    where
        p True = putStrLn ("Ignoring" ++ show x)
        p False = return ()

-- TODO: replace with the function of gui
checkAst :: Bool -> IO [FuncDeclaration] -> IO [FuncDeclaration]
checkAst _ funcsIo =
    funcsIo
        >>= (\funcs -> case typeCheck funcs of
            True -> return funcs
            False -> fail "Invalid Code")

transformToWatLike :: Bool -> IO [FuncDeclaration] -> IO [FuncDeclare]
transformToWatLike v funcsIo =
    p v
    >> funcsIo
        >>= return . aSTToWatLike
    where
        p True = putStrLn "Transforming Leviator AST to IR (WatLike)..."
        p False = return ()

transformToWat :: Bool -> IO [FuncDeclare] -> IO [FuncDef]
transformToWat v funcsIo =
    p v
    >> funcsIo
        >>= return . watsLikeToWat
    where
        p True = putStrLn "Transforming IR (WatLike) to IR (Wat)..."
        p False = return ()

transformToWasm :: Bool -> IO [FuncDef] -> IO Wasm
transformToWasm v funcsIo =
    p v
    >> funcsIo
        >>= return . watToWasm
    where
        p True = putStrLn "Transforming IR (Wat) to Wasm..."
        p False = return ()

showDebug :: Bool -> Wasm -> IO ()
showDebug True wasm = print wasm
showDebug False _ = return ()

run :: Args -> IO ()
run (Args Run fPath oFile v fPaths) =
    transformedWasm >>= \wasm -> (showDebug v wasm >> writeWasm wasm oFile)
    where
        expressions = listsAllFiles v (fPath:fPaths) >>= getFilesExpression v
        funcs = expressions >>= getAllFunc v
        transformedWatLike = transformToWatLike v (checkAst v funcs)
        transformedWat = transformToWat v transformedWatLike
        transformedWasm = transformToWasm v transformedWat
run _ = fail "Invalid option called"
