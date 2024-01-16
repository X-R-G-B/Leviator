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
getVectorSize cntent = getLEB128ToI64 cntent

extractTypes :: (Int64, Bs.ByteString) -> ([TypeName], Bs.ByteString)
extractTypes (0, cntent) = ([], cntent)
extractTypes (idx, cntent) =
  (getTypeFromByte (head $ Bs.unpack cntent) : typs, rest)
    where (typs, rest) = extractTypes (idx - 1, Bs.drop 1 cntent)

parseFuncType :: Int32 -> Bs.ByteString -> (FuncType, Bs.ByteString)
parseFuncType idtfier cntent = (FuncType idtfier prams res, rest2)
  where
    (prams, rest) = extractTypes (getVectorSize cntent)
    (res, rest2) = extractTypes (getVectorSize rest)

parseFuncTypes :: Int32 -> Int64 -> Bs.ByteString -> [FuncType]
parseFuncTypes idx maxIdx cntent
  | idx >= (fromIntegral maxIdx) = []
  | head (Bs.unpack cntent) == 0x60 =
    fnType : parseFuncTypes (idx + 1) maxIdx rest
  | otherwise = throw $ WasmError "ParseFuncTypes: 0x60 expected for function"
  where (fnType, rest) = parseFuncType idx (Bs.drop 1 cntent)

getFuncTypes :: Section -> [FuncType]
getFuncTypes (Section TypeID _ cntent) = parseFuncTypes 0 vecSize rest
  where (vecSize, rest) = getLEB128ToI64 cntent
getFuncTypes _ = throw $ WasmError "getFuncTypes: bad section"
