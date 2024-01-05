{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Main
-}

module Main (main) where

import WasmMod.Module

main :: IO ()
main = do
    wasmMod <- loadModule "./test/test.wasm"
    print wasmMod
