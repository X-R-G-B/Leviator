{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Main
-}

module Main (main) where

import Expression
import Parser

main :: IO ()
main = print $ runParser (parseExpresion)
    "fn main () -> Int \n{\n    <- 0;\n};\n"
-- main = print $ runParser (parseExpresion) "alias abc def;\n"
-- main = print $ runParser (parseExpresion) "// this is a comment\n"
