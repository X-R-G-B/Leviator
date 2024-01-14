{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Memory
-}

module Parsing.Memory
(
  getMemories
) where

import Control.Exception (throw)
import qualified Data.ByteString.Lazy as BS (ByteString, drop, unpack, empty)

import Types
import Leb128 (getLEB128ToI32)
import Errors (CustomException(..))

parseMinMax :: BS.ByteString -> Memory
parseMinMax content
  | endBs /= BS.empty = throw $ WasmError "parseMinMax: bad memory section"
  | otherwise = Limit {lMin = min, lMax = Just max}
  where
    (min, rest) = getLEB128ToI32 content
    (max, endBs) = getLEB128ToI32 rest

parseMin :: BS.ByteString -> Memory
parseMin content
  | endBs /= BS.empty = throw $ WasmError "parseMin: bad memory section"
  | otherwise = Limit {lMin = min, lMax = Nothing}
  where
    (min, endBs) = getLEB128ToI32 content

parseMemory :: BS.ByteString -> Memory
parseMemory content
  | head (BS.unpack content) == 0x01 = parseMinMax (BS.drop 1 content)
  | head (BS.unpack content) == 0x00 = parseMin (BS.drop 1 content)
  | otherwise = throw $ WasmError "parseMemory: bad memory section"

getMemories :: Section -> Memory
getMemories (Section MemoryID _ content)
  | head (BS.unpack content) == 0x01 = parseMemory (BS.drop 1 content)
  | otherwise = throw $ WasmError "getMemories: v1 allow 1 memory only"
getMemories _ = throw $ WasmError "getMemories: bad memory section"
