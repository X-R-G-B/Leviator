{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

main :: IO ()
main = putStrLn (showMaybeTree (textToAST "(fst 1 (scd 2 3 4) 12)"))
