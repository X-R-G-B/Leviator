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

import Data.Int (Int64, Int32)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy as Bs

import Types
import Leb128 (getLEB128ToI64)
import Errors (CustomException(..))

getVectorSize :: Bs.ByteString -> (Int64, Bs.ByteString)
getVectorSize content = getLEB128ToI64 content

extractTypes :: (Int64, Bs.ByteString) -> ([TypeName], Bs.ByteString)
extractTypes (0, content) = ([], content)
extractTypes (idx, content) =
  (getTypeFromByte (head $ Bs.unpack content) : types, rest)
    where (types, rest) = extractTypes (idx - 1, Bs.drop 1 content)

parseFuncType :: Int32 -> Bs.ByteString -> (FuncType, Bs.ByteString)
parseFuncType id content = (FuncType id params results, rest2)
  where
    (params, rest) = extractTypes (getVectorSize content)
    (results, rest2) = extractTypes (getVectorSize rest)

parseFuncTypes :: Int32 -> Int64 -> Bs.ByteString -> [FuncType]
parseFuncTypes idx maxIdx content
  | idx >= (fromIntegral maxIdx) = []
  | head (Bs.unpack content) == 0x60 =
    funcType : parseFuncTypes (idx + 1) maxIdx rest
  | otherwise = throw $ WasmError "ParseFuncTypes: 0x60 expected for function"
  where (funcType, rest) = parseFuncType idx (Bs.drop 1 content)

getFuncTypes :: Section -> [FuncType]
getFuncTypes (Section TypeID _ content) = parseFuncTypes 0 vecSize rest
  where (vecSize, rest) = getLEB128ToI64 content
getFuncTypes _ = throw $ WasmError "getFuncTypes: bad section"
