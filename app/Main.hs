{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import AST
import TextToAST

main :: IO ()
main = putStrLn (showMaybeTree (textToAST "(fst 1 (scd 2 3 4) 12)"))
