{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Start
-}

module Run.Start
  (
    start
  )
where

import Types
import Errors

start :: WasmModule -> IO ()
start wasmMod = do
  putStrLn "Start"
  print wasmMod
  putStrLn "End"
