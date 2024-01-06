{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Errors
-}
 
module Errors
  (
    CustomException(..),
    handleException
  )
where

import Control.Exception (Exception(..), SomeException, displayException)

data CustomException =
    ParseError String |
    WasmError String |
    RuntimeError String
  deriving (Show, Eq)

instance Exception CustomException

handleException :: SomeException -> IO ()
handleException e = putStrLn $ "Error: " ++ displayException e
