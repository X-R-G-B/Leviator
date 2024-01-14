{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Types
-}

module Parsing.FuncTypes
  (
    getFuncTypes
  )
where

import qualified Data.ByteString.Lazy as Bs
import Control.Exception (throw)
import Data.Int (Int64, Int32)
import Data.Word (Word8)

import Leb128
import Errors
import Types

getVectorSize :: Bs.ByteString -> (Int64, Bs.ByteString)
getVectorSize content = getLEB128ToI64 content

extractTypes :: (Int64, Bs.ByteString) -> ([TypeName], Bs.ByteString)
extractTypes (0, content) = ([], content)
extractTypes (idx, content) = (getTypeFromByte (head $ Bs.unpack content) : types, rest)
  where (types, rest) = extractTypes (idx - 1, Bs.drop 1 content)

parseFuncType :: Int32 -> Bs.ByteString -> (FuncType, Bs.ByteString)
parseFuncType id content = do
  let (params, rest) = extractTypes (getVectorSize content)
  let (results, rest2) = extractTypes (getVectorSize rest)
  ((FuncType id params results), rest2)

parseFuncTypes :: Int32 -> Int64 -> Bs.ByteString -> [FuncType]
parseFuncTypes idx maxIdx content
  | idx >= (fromIntegral maxIdx) = []
  | head (Bs.unpack content) == 0x60 = do
    let (funcType, rest) = parseFuncType idx (Bs.drop 1 content)
    funcType : parseFuncTypes (idx + 1) maxIdx rest
  | otherwise = throw $ WasmError "ParseFuncTypes: 0x60 expected for function"

getFuncTypes :: Section -> [FuncType]
getFuncTypes (Section TypeID _ content) = do
  let (vecSize, rest) = getLEB128ToI64 content
  parseFuncTypes 0 vecSize rest
getFuncTypes _ = throw $ WasmError "getFuncTypes: bad section"
