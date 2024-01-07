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

import qualified Data.ByteString.Lazy as BS (ByteString, unpack, readFile)
import Control.Exception (throwIO, throw)
import Control.Monad (when)
import Numeric (showHex)

import Errors
import WasmMod.Header
import WasmMod.Sections
import WasmMod.Sections.Types
import WasmMod.Sections.Global

data WasmModule = WasmModule {
  header :: ModHeader,
  sections :: [Section]
}

instance Show WasmModule where
  show wasmMod = "\n[ Wasm Module Header ]\n" ++
    "- Magic Number: " ++ (concat $ map (\x -> showHex x " ") $
      BS.unpack $ magicNumber $ header wasmMod) ++ "\n" ++
    "- Version: " ++ (concat $ map (\x -> showHex x " ") $
      BS.unpack $ version $ header wasmMod) ++ "\n" ++
    "- Sections: " ++ (show $ sections wasmMod) ++ "\n"

getFileContent :: String -> IO BS.ByteString
getFileContent path = BS.readFile path

--TEMP FUNC
getGlobalSection :: [Section] -> Section
getGlobalSection [] = throw (WasmError "No global section")
getGlobalSection (x:xs)
  | identifier x == GlobalID = x
  | otherwise = getGlobalSection xs

loadModule :: String -> IO WasmModule
loadModule filePath = do
  bytes <- getFileContent filePath
  let modHeader = getModHeader bytes
  when (not $ isHeaderValid modHeader) $ throwIO (WasmError "Invalid header")
  let modSections = getModSections bytes
  when (not $ areSectionsValid modSections) $ throwIO (WasmError "Invalid sections")
  let funcType = parseTypes $ head modSections
  print funcType
  let globals = parseGlobals $ getGlobalSection modSections
  print globals
  return $ WasmModule modHeader modSections
