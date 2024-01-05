{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Module
-}

module WasmMod.Module
  (
    WasmModule(..),
    loadModule
  )
where

import qualified Data.ByteString as BS (ByteString, unpack, readFile)
import Numeric (showHex)

import WasmMod.Header
import WasmMod.Sections

data WasmModule = WasmModule {
  header :: ModHeader,
  sections :: [Section]
}

instance Show WasmModule where
  show wasmMod = "Wasm Module Header:\n" ++
    "  Magic Number: " ++ (concat $ map (\x -> showHex x " ") $
      BS.unpack $ magicNumber $ header wasmMod) ++ "\n" ++
    "  Version: " ++ (concat $ map (\x -> showHex x " ") $
      BS.unpack $ version $ header wasmMod) ++ "\n" ++
    "  Sections: " ++ (show $ sections wasmMod) ++ "\n"

getFileContent :: String -> IO BS.ByteString
getFileContent path = BS.readFile path

loadModule :: String -> IO WasmModule
loadModule filePath = do
    bytes <- getFileContent filePath
    return $ WasmModule (getModuleHeader bytes) []
