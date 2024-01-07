{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Types
-}

module WasmMod.Sections.Types
  (
    parseTypes,
    Type(..),
    FuncType(..)
  )
where

import qualified Data.ByteString.Lazy as Bs
import Control.Exception (throw)
import Data.Int (Int64)
import Data.Word (Word8)

import WasmMod.Sections
import WasmMod.Leb128
import Errors

data Type = I32 | I64 | F32 | F64 deriving (Show, Eq)

data FuncType = FuncType {
  typeId :: Int,
  params :: [Type],
  results :: [Type]
}

instance Show FuncType where
  show funcType = "(type " ++ (show $ typeId funcType) ++ " (func " ++
    (show $ params funcType) ++ ") " ++ (show $ results funcType) ++ ")\n"

getVectorSize :: Bs.ByteString -> (Int64, Bs.ByteString)
getVectorSize content = extractLEB128 content

getTypeFromByte :: Word8 -> Type
getTypeFromByte 0x7f = I32
getTypeFromByte 0x7e = I64
getTypeFromByte 0x7d = F32
getTypeFromByte 0x7c = F64
getTypeFromByte _ = throw $ WasmError "GetTypeFromByte: bad type"

extractTypes :: (Int64, Bs.ByteString) -> ([Type], Bs.ByteString)
extractTypes (0, content) = ([], content)
extractTypes (idx, content) = (getTypeFromByte (head $ Bs.unpack content) : types, rest)
  where (types, rest) = extractTypes (idx - 1, Bs.drop 1 content)

parseFuncType :: Int -> Bs.ByteString -> (FuncType, Bs.ByteString)
parseFuncType id content = do
  let (params, rest) = extractTypes (getVectorSize content)
  let (results, rest2) = extractTypes (getVectorSize rest)
  ((FuncType id params results), rest2)

parseFuncTypes :: Int -> Int64 -> Bs.ByteString -> [FuncType]
parseFuncTypes idx maxIdx content
  | idx >= (fromIntegral maxIdx) = []
  | head (Bs.unpack content) == 0x60 = do
    let (funcType, rest) = parseFuncType idx (Bs.drop 1 content)
    funcType : parseFuncTypes (idx + 1) maxIdx rest
  | otherwise = throw $ WasmError "ParseFuncTypes: 0x60 expected for function"

parseTypes :: Section -> [FuncType]
parseTypes (Section TypeID _ content) = do
  let (vecSize, rest) = extractLEB128 content
  parseFuncTypes 0 vecSize rest
parseTypes _ = []
