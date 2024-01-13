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
import Args

import System.Directory (listDirectory)
import System.FilePath (joinPath)
import Data.List (isSuffixOf)

getExpressionFromFile :: FilePath -> IO [Expression]
getExpressionFromFile path =
    readFile path
    >>= (\str ->
        case runParser (proceedAlias <$> parseAllExpression) str of
            Nothing -> fail "Invalid expression"
            Just (expression, _) -> return expression)

getFilesExpression :: [FilePath] -> IO [Expression]
getFilesExpression (file:files) =
    getExpressionFromFile file
        >>= (\expFile -> getFilesExpression files
            >>= (\expFiles -> return (expFile ++ expFiles)))
getFilesExpression [] = return []

selectGoodFiles :: FilePath -> [FilePath] -> IO [FilePath]
selectGoodFiles folder [] = return []
selectGoodFiles folder (file:files)
    | ".lvt" `isSuffixOf` trueFile =
            putStrLn ("- " ++ trueFile)
            >> selectGoodFiles folder files
                >>= (\others -> return (trueFile : others))
    | otherwise = selectGoodFiles folder files
    where
        trueFile = joinPath [folder, file]

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles path = listDirectory path >>= selectGoodFiles path

getAllFunc :: [Expression] -> IO [FuncDeclaration]
getAllFunc [] = return []
getAllFunc ((Expression.Function str):expressions) =
    case runParser parseFuncDeclaration str of
        Nothing -> fail ("Parser Error: `" ++ str ++ "`")
        Just (func, _) ->
            getAllFunc expressions >>= \funcs -> return (func:funcs)
getAllFunc (_ : expressions) = getAllFunc expressions

-- TODO: replace with the function of gui
checkAst :: IO [FuncDeclaration] -> IO [FuncDeclaration]
checkAst funcsIo =
    funcsIo
        >>= (\funcs -> case Just funcs of
            Just f -> return f
            Nothing -> fail "Invalid Code")

transformToWatLike :: IO [FuncDeclaration] -> IO [FuncDeclare]
transformToWatLike funcsIo =
    funcsIo
        >>= (\funcs -> return (aSTToWatLike funcs))

run :: Args -> IO ()
run (Args Run fPath oFile) = putStrLn ("Compiling from: " ++ fPath) >>
    transformed >>= print
    where
        expressions = listAllFiles fPath >>= getFilesExpression
        funcs = expressions >>= getAllFunc
        transformed = transformToWatLike (checkAst funcs)
run _ = fail "Invalid option called"
