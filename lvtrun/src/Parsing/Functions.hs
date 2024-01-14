{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Functions
-}

module Parsing.Functions
(
  getFunctions
)
where

import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throw)
import Data.Int (Int64, Int32)

import Types
import Errors
import Leb128

parseFunctionsIndex :: Int32 -> Int64 -> BSL.ByteString -> [Function]
parseFunctionsIndex idtfier maxIdx cntent
  | idtfier > (fromIntegral maxIdx) = []
  | BSL.length cntent == 0 = []
  | otherwise =
    Function {
      funcType = typeIdx,
      funcIdx = idtfier,
      body = [],
      locals = []
    } : parseFunctionsIndex (idtfier + 1) maxIdx rest
  where (typeIdx, rest) = getLEB128ToI32 cntent

getFunctions :: Section -> [Function]
getFunctions (Section FunctionID _ cntent) =
  parseFunctionsIndex 0 vecSize rest
  where (vecSize, rest) = getLEB128ToI64 cntent
getFunctions _ = throw $ WasmError "getFunctions: bad section"
