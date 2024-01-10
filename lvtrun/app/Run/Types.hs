{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Types
-}

module Run.Types
  (
    VMConfig(..),
  )
where

import Types

data VMConfig = VMConfig {
  start :: Bool
} deriving (Show)
