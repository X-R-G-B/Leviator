{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Abstract Syntax Tree
-}

module AST
    (
        showMaybeTree
    ) where

import Types

showMaybeTree :: Maybe Tree -> String
showMaybeTree Nothing = "Nothing"
showMaybeTree (Just tree) = show tree
