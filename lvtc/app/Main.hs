{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Main
-}

module Main (main) where

import Expression (parseExpresion)
import Parser (runParser)
import ParseLvt (parseInstruction, parseInstructions)

test1 :: String
test1 = "if (a)\n{\nb(0);\n};\n"

test2 :: String
test2 = part1 ++ part2
    where
        part1 = "@Int a = 0;\n    @Int c = b(a);\n"
        part2 = "    if (c)\n    {\n        d(a);\n    };\n"

test3 :: String
test3 = "alias abc def;\n"

test4 :: String
test4 = "// this is a comment\n"

main :: IO ()
main =
    print (runParser parseInstruction test1)
    >> print (runParser parseInstructions test2)
    >> print (runParser parseExpresion test3)
    >> print (runParser parseExpresion test4)
