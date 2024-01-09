
module WasmMod.Sections.Memory
  (
    Memory(..),
    parseMemory
  ) where

import qualified Data.ByteString.Lazy as BS
import Control.Exception (throw)
import Control.Monad (when)

import WasmMod.Leb128
import WasmMod.Sections.Types
import WasmMod.Sections
import Errors

data Memory = Memory { memMin :: Int, memMax :: Maybe Int }
  deriving (Show, Eq)

parseMinMax :: BS.ByteString -> Memory
parseMinMax content
  | endBs /= BS.empty = throw $ WasmError "parseMinMax: bad memory section"
  | otherwise = Memory {memMin = fromIntegral min, memMax = Just (fromIntegral max)}
  where
    (min, rest) = extractLEB128 content
    (max, endBs) = extractLEB128 rest

parseMin :: BS.ByteString -> Memory
parseMin content
  | endBs /= BS.empty = throw $ WasmError "parseMin: bad memory section"
  | otherwise = Memory {memMin = fromIntegral min, memMax = Nothing}
  where
    (min, endBs) = extractLEB128 content

parseMemory' :: BS.ByteString -> Memory
parseMemory' content
  | head (BS.unpack content) == 0x01 = parseMinMax (BS.drop 1 content)
  | head (BS.unpack content) == 0x00 = parseMin (BS.drop 1 content)
  | otherwise = throw $ WasmError "parseMemory': bad memory section"

parseMemory :: Section -> Memory
parseMemory (Section MemoryID _ content)
  | head (BS.unpack content) == 0x01 = parseMemory' (BS.drop 1 content)
  | otherwise = throw $ WasmError "parseMemory: v1 allow 1 memory only"
parseMemory _ = throw $ WasmError "parseMemory: bad memory section"

--https://webassembly.github.io/spec/core/exec/runtime.html#memory-instances
