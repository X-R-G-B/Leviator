{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Main
-}

module Main (main) where

import Control.Exception (try)
import Errors
import Loader

main :: IO ()
main = try (loadModule "./test/simple.wasm") >>= \result ->
    case result of
        Left err -> handleException err
        Right wasmMod -> print wasmMod
