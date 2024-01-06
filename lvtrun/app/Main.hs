{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Main
-}


module Main (main) where

import Control.Exception (try)
import WasmMod.Module
import Errors

main :: IO ()
main = try (loadModule "./test/test.wasm") >>= \result ->
    case result of
        Left err -> handleException err
        Right wasmMod -> print wasmMod
