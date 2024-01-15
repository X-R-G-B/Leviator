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
import Data.Int (Int64, Int32)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy as Bs

import Types
import Leb128 (getLEB128ToI64, getLEB128ToI32)
import Errors (CustomException(WasmError))

getExportNb :: Bs.ByteString -> (Int64, Bs.ByteString)
getExportNb cntent = getLEB128ToI64 cntent

word8ToString :: [Word8] -> String
word8ToString = map (chr . fromIntegral)

createExport :: [Word8] -> Word8 -> FuncIdx -> Export
createExport nme 0x00 idx = Export (word8ToString nme) (ExportFunc idx)
createExport nme 0x01 idx = Export (word8ToString nme) (ExportTable idx)
createExport nme 0x02 idx = Export (word8ToString nme) (ExportMemory idx)
createExport nme 0x03 idx = Export (word8ToString nme) (ExportGlobal idx)
createExport _ _ _ = throw $ WasmError "createExport: bad export"

parseExports :: Int32 -> Int64 -> Bs.ByteString -> [Export]
parseExports idx maxIdx cntent
  | idx >= (fromIntegral maxIdx) = []
  | Bs.length cntent == 0 = []
  | otherwise = export : parseExports (idx + 1) maxIdx rest3
  where
    (nameLen, rest) = getLEB128ToI64 cntent
    (nme, rest2) = Bs.splitAt nameLen rest
    exportType = head (Bs.unpack rest2)
    (exportValue, rest3) = getLEB128ToI32 (Bs.drop 1 rest2)
    export = createExport (Bs.unpack nme) exportType exportValue

getExports :: Section -> [Export]
getExports (Section ExportID _ cntent) = parseExports 0 exprtsNb rest
  where
    (exprtsNb, rest) = getExportNb cntent
getExports _ = throw $ WasmError "getExports: bad section"
