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
parseMinMax cntent
  | endBs /= BS.empty = throw $ WasmError "parseMinMax: bad memory section"
  | otherwise = Limit {lMin = memMin, lMax = Just memMax}
  where
    (memMin, rest) = getLEB128ToI32 cntent
    (memMax, endBs) = getLEB128ToI32 rest

parseMin :: BS.ByteString -> Memory
parseMin cntent
  | endBs /= BS.empty = throw $ WasmError "parseMin: bad memory section"
  | otherwise = Limit {lMin = memMin, lMax = Nothing}
  where
    (memMin, endBs) = getLEB128ToI32 cntent

parseMemory :: BS.ByteString -> Memory
parseMemory cntent
  | head (BS.unpack cntent) == 0x01 = parseMinMax (BS.drop 1 cntent)
  | head (BS.unpack cntent) == 0x00 = parseMin (BS.drop 1 cntent)
  | otherwise = throw $ WasmError "parseMemory: bad memory section"

getMemories :: Section -> Memory
getMemories (Section MemoryID _ cntent)
  | head (BS.unpack cntent) == 0x01 = parseMemory (BS.drop 1 cntent)
  | otherwise = throw $ WasmError "getMemories: v1 allow 1 memory only"
getMemories _ = throw $ WasmError "getMemories: bad memory section"
