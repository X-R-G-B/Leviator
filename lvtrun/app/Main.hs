{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Main
-}

module Main (main) where

import qualified Data.ByteString as BS
import WasmMod.Module
import WasmMod.Header

getFileContent :: String -> IO BS.ByteString
getFileContent path = BS.readFile path

main :: IO ()
main = do
    wasmFile <- getFileContent "./test/test.wasm"
    let wasmMod = loadModule wasmFile
    print wasmMod
    if isHeaderValid $ header wasmMod
        then putStrLn "Header is valid"
        else putStrLn "Header is invalid"
