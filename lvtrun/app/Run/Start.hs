{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Code
-}

module Run.Start
(
  startExecution
)
where

import Types
import Run.Vm
import Run.Functions
import Run.Stack

exitCorrectly :: Stack -> IO ()
exitCorrectly [] = putStrLn "Exit correctly with code: 0"
exitCorrectly (x:_) = putStrLn $ "Exit correctly with code: " ++ show x

startExecution :: WasmModule -> IO ()
startExecution wasmMod = exitCorrectly $ vmAtEnd
  where
    vmAtEnd = runMain (createVm wasmMod) startFuncId
    startFuncId = getStartFunctionId (exports wasmMod)
