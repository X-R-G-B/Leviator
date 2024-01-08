{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)

import Args (Args (..), parseArgs, Action (..), printHelp)
import Run (run)
import Version (printVersion)

dispatchArgs :: Args -> IO ()
dispatchArgs (Args Run fPath oFile) = run (Args Run fPath oFile)
dispatchArgs (Args ShowHelp _ _) = printHelp
dispatchArgs (Args ShowVersion _ _) = printVersion
-- dispatchArgs (Args New _ _) =

dispatchIfOk :: Either Args String -> IO ()
dispatchIfOk (Left args) = dispatchArgs args
dispatchIfOk (Right str) = print str

main :: IO ()
main = getArgs >>= parseArgs >>= dispatchIfOk
