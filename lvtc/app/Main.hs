{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Main
-}

module Main (main) where

import Expression
import Parser
import ParseLvt (parseInstruction)

main :: IO ()
main =
    print (runParser parseInstruction "if (a)\n{\nb(0);\n};\n")
    >> print (runParser parseExpresion "alias abc def;\n")
    >> print (runParser parseExpresion "// this is a comment\n")
