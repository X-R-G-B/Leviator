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

data Type = I32 | I64 | F32 | F64 deriving (Show, Eq)

data FuncType = FuncType {
  typeId :: Int,
  params :: [Type],
  results :: [Type]
}

instance Show FuncType where
  show funcType = "(type " ++ (show $ typeId funcType) ++ " (func " ++
    (show $ params funcType) ++ ") " ++ (show $ results funcType) ++ ")"

parseFuncTypes :: Int64 -> Bs.ByteString -> [FuncType]
parseFuncTypes 0 _ = []
parseFuncTypes idx content = throw $ (WasmError "Not implemented")

parseTypes :: Section -> [FuncType]
parseTypes (Section TypeID _ content) = do
  let (vecSize, rest) = extractLEB128 content
  parseFuncTypes vecSize rest
parseTypes _ = []
