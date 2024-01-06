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

import WasmMod.Sections
import WasmMod.Leb128
import Data.Int
import Errors
import Control.Exception
import qualified Data.ByteString.Lazy as Bs
import Data.Word

import Debug.Trace

data Type = I32 | I64 | F32 | F64 deriving (Show, Eq)

data FuncType = FuncType {
  typeId :: Int,
  params :: [Type],
  results :: [Type]
}

instance Show FuncType where
  show funcType = "(type " ++ (show $ typeId funcType) ++ " (func " ++
    (show $ params funcType) ++ ") " ++ (show $ results funcType) ++ ")"

--60 0 0
--60 1 7f 0
--60 2 7f 7f 1 7f
-- 7f = i32 7e = i64 7d = f32 7c = f64

getVectorSize :: Bs.ByteString -> (Int64, Bs.ByteString)
getVectorSize content = extractLEB128 content

getTypeFromByte :: Word8 -> Type
getTypeFromByte 0x7f = I32
getTypeFromByte 0x7e = I64
getTypeFromByte 0x7d = F32
getTypeFromByte 0x7c = F64
getTypeFromByte _ = throw $ WasmError "GetTypeFromByte: bad type"

extractParams :: (Int64, Bs.ByteString) -> ([Type], Bs.ByteString)
extractParams (0, content) = ([], content)
extractParams (idx, content) = (getTypeFromByte (head $ Bs.unpack content) : params, rest)
  where (params, rest) = extractParams (idx - 1, Bs.drop 1 content)

extractResults :: (Int64, Bs.ByteString) -> ([Type], Bs.ByteString)
extractResults (0, content) = ([], content)
extractResults (idx, content) = (getTypeFromByte (head $ Bs.unpack content) : results, rest)
  where (results, rest) = extractResults (idx - 1, Bs.drop 1 content)

extractTypes :: (Int64, Bs.ByteString) -> ([Type], Bs.ByteString)
extractTypes (0, content) = ([], content)

parseFuncType :: Bs.ByteString -> (FuncType, Bs.ByteString)
parseFuncType content = do
  let (params, rest) = extractParams $ getVectorSize content
  let (results, rest2) = extractResults $ getVectorSize rest
  ((FuncType 0 params results), rest2)

parseFuncTypes :: Int64 -> Bs.ByteString -> [FuncType]
parseFuncTypes 0 _ = []
parseFuncTypes idx content
  | head (Bs.unpack content) == 0x60 = do
    let (funcType, rest) = parseFuncType content
    funcType : parseFuncTypes (idx - 1) rest
  | otherwise = throw $ WasmError "ParseFuncTypes: 0x60 expected for function"

parseTypes :: Section -> [FuncType]
parseTypes (Section TypeID _ content) = do
  let (vecSize, rest) = extractLEB128 content
  parseFuncTypes vecSize rest
parseTypes _ = []
