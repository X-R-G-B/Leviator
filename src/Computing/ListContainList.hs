{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ListContainList
-}

module Computing.ListContainList 
    (
        doesListContainsList
    ) where

import Types

doesListContainsList :: [Tree] -> Bool
doesListContainsList [] = False
doesListContainsList (List _ : _) = True
doesListContainsList (_ : rest) = doesListContainsList rest
