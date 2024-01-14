{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- IO
-}

module IO
(
  getFileContent
)
where

import qualified Data.ByteString.Lazy as BSL (readFile)

import Types

getFileContent :: String -> IO FileContent
getFileContent path = BSL.readFile path
