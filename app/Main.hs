{-
-- EPITECH PROJECT, 2023
-- Main
-- File description:
-- Main
-}

module Main (main) where

import AST (showMaybeTree)
import TextToAST (textToAST)

main :: IO ()
main = putStrLn (showMaybeTree (textToAST "(fst 1 (scd 2 3 4) 12)"))
