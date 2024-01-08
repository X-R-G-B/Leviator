{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Run
-}

module Version
(
    printVersion
) where

import LvtLibVersion

printVersion :: IO ()
printVersion = putStrLn lvtLibVersion
