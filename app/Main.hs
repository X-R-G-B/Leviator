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
main = putStrLn (showMaybeTree (textToAST "(define javascriptIsGood #f)"))
