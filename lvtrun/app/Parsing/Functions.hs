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
parseFunctionsIndex idx maxIdx content
  | idx > (fromIntegral maxIdx) = []
  | BSL.length content == 0 = []
  | otherwise = do
    let (typeIdx, rest) = getLEB128ToI32 content
    Function {
      funcType = fromIntegral typeIdx,
      funcIdx = idx,
      body = []
    } : parseFunctionsIndex (idx + 1) maxIdx rest

getFunctions :: Section -> [Function]
getFunctions (Section FunctionID _ content) = do
  let (vecSize, rest) = getLEB128ToI64 content
  parseFunctionsIndex 0 vecSize rest
getFunctions _ = throw $ WasmError "getFunctions: bad section"
