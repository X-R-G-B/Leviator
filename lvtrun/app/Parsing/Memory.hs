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

import qualified Data.ByteString.Lazy as BS
import Control.Exception (throw)
import Control.Monad (when)

import Leb128
import Types
import Errors

parseMinMax :: BS.ByteString -> Memory
parseMinMax content
  | endBs /= BS.empty = throw $ WasmError "parseMinMax: bad memory section"
  | otherwise = Limit {lMin = fromIntegral min, lMax = Just (fromIntegral max)}
  where
    (min, rest) = extractLEB128 content
    (max, endBs) = extractLEB128 rest

parseMin :: BS.ByteString -> Memory
parseMin content
  | endBs /= BS.empty = throw $ WasmError "parseMin: bad memory section"
  | otherwise = Limit {lMin = fromIntegral min, lMax = Nothing}
  where
    (min, endBs) = extractLEB128 content

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

--https://webassembly.github.io/spec/core/exec/runtime.html#memory-instances
