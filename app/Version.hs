{-
-- EPITECH PROJECT, 2023
-- koaky
-- File description:
-- version
-}

module Version
    ( printVersion
    ) where

import KoakyLibVersion

printVersion :: IO ()
printVersion = putStrLn koakyLibVersion
