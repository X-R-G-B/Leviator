{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Types
-}

module Types
  (
    Type(..),
    getTypeFromByte
  )
  where

import Data.Word (Word8)
import Control.Exception (throw)

import Errors

data Type = I32 | I64 | F32 | F64 deriving (Show, Eq)

getTypeFromByte :: Word8 -> Type
getTypeFromByte 0x7f = I32
getTypeFromByte 0x7e = I64
getTypeFromByte 0x7d = F32
getTypeFromByte 0x7c = F64
getTypeFromByte _ = throw $ WasmError "GetTypeFromByte: bad type"
