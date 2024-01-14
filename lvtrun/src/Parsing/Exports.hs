{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Exports
-}

module Parsing.Exports
(
  getExports
)
where

import Data.Char (chr)
import Data.Word (Word8)
import Control.Monad (when)
import Data.Int (Int64, Int32)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy as Bs

import Types
import Leb128 (getLEB128ToI64, getLEB128ToI32)
import Errors (CustomException(WasmError))

isExportValid :: Word8 -> Bool
isExportValid 0x00 = True
isExportValid 0x01 = True
isExportValid 0x02 = True
isExportValid 0x03 = True
isExportValid _ = False

getExportNb :: Bs.ByteString -> (Int64, Bs.ByteString)
getExportNb content = getLEB128ToI64 content

word8ToString :: [Word8] -> String
word8ToString = map (chr . fromIntegral)

createExport :: [Word8] -> Word8 -> FuncIdx -> Export
createExport name 0x00 idx = Export (word8ToString name) (ExportFunc idx)
createExport name 0x01 idx = Export (word8ToString name) (ExportTable idx)
createExport name 0x02 idx = Export (word8ToString name) (ExportMemory idx)
createExport name 0x03 idx = Export (word8ToString name) (ExportGlobal idx)
createExport _ _ _ = throw $ WasmError "createExport: bad export"

parseExports :: Int32 -> Int64 -> Bs.ByteString -> [Export]
parseExports idx maxIdx content
  | idx >= (fromIntegral maxIdx) = []
  | Bs.length content == 0 = []
  | otherwise = export : parseExports (idx + 1) maxIdx rest3
  where
    (nameLen, rest) = getLEB128ToI64 content
    (name, rest2) = Bs.splitAt nameLen rest
    exportType = head (Bs.unpack rest2)
    (exportValue, rest3) = getLEB128ToI32 (Bs.drop 1 rest2)
    export = createExport (Bs.unpack name) exportType exportValue

getExports :: Section -> [Export]
getExports (Section ExportID _ content) = parseExports 0 exprtsNb rest
  where
    (exprtsNb, rest) = getExportNb content
getExports _ = throw $ WasmError "getExports: bad section"
