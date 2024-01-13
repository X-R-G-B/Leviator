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
import Data.Word (Word8)

import Types
import Errors
import Leb128

import Debug.Trace

parseFunctionsIndex :: Int32 -> Int64 -> BSL.ByteString -> [Function]
parseFunctionsIndex idx maxIdx content
  | idx > (fromIntegral maxIdx) = []
  | BSL.length content == 0 = []
  | otherwise = do
    let (typeIdx, rest) = extractLEB1282 content
    Function {funcType = fromIntegral typeIdx, funcIdx = idx, body = []} : parseFunctionsIndex (idx + 1) maxIdx rest

getFunctions :: Section -> [Function]
getFunctions (Section FunctionID _ content) = do
  let (vecSize, rest) = extractLEB128 content
  parseFunctionsIndex 0 vecSize rest
getFunctions _ = throw $ WasmError "getFunctions: bad section"
