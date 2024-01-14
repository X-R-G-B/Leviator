{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Global
-}

module Parsing.Global
  (
    getGlobals,
  )
where

import Data.Int (Int64)
import Data.Word (Word8)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import Types
import Errors (CustomException(..))
import Leb128 (getLEB128ToI64)
import OpCodes (extractOpCode, createInstruction)

parseInstruction :: BSL.ByteString -> (Instruction, BSL.ByteString)
parseInstruction bytes
  | BSL.length bytes == 0 =
    throw $ WasmError "ParseInstruction: no instruction"
  | otherwise = (instruction, rest2)
  where
    (opCode, rest) = extractOpCode bytes
    (instruction, rest2) = createInstruction opCode rest

parseInstructions :: BSL.ByteString -> [Instruction]
parseInstructions bytes
  | BSL.length bytes == 0 = []
  | head (BSL.unpack bytes) == 0x0b = []
  | otherwise = instruction : parseInstructions rest
  where
    (instruction, rest) = parseInstruction bytes

parseMutability :: Word8 -> Mutability
parseMutability 0x00 = Const
parseMutability 0x01 = Var
parseMutability _ = throw $ WasmError "ParseMutability: bad mutability"

getHexaIndex :: BSL.ByteString -> Int64 -> Int64
getHexaIndex cntent idx
  | idx >= BSL.length cntent =
    throw $ WasmError "GetHexaIndex: no 0x0b found"
  | (head $ BSL.unpack $ BSL.drop idx cntent) == 0x0b = idx
  | otherwise = getHexaIndex cntent (idx + 1)

extractExpression :: BSL.ByteString -> (BSL.ByteString, BSL.ByteString)
extractExpression cntent = (expression, rest)
  where
    idx = getHexaIndex cntent 0
    expression = BSL.take (idx + 1) cntent
    rest = BSL.drop (idx + 1) cntent

parseGlobal :: BSL.ByteString -> (Global, BSL.ByteString)
parseGlobal cntent = (Global gblType mtability instructions, rest)
  where
    gblType = getTypeFromByte (head $ BSL.unpack cntent)
    mtability = parseMutability (head $ BSL.unpack $ BSL.drop 1 cntent)
    (expression, rest) = extractExpression (BSL.drop 2 cntent)
    instructions = parseInstructions expression

parseGlobals :: Int64 -> Int64 -> BSL.ByteString -> [Global]
parseGlobals idx maxIdx cntent
  | idx >= maxIdx = []
  | otherwise = global : parseGlobals (idx + 1) maxIdx rest
  where
    (global, rest) = parseGlobal cntent

getGlobals :: Section -> [Global]
getGlobals (Section GlobalID _ cntent) =
  parseGlobals 0 vecSize rest
  where
    (vecSize, rest) = getLEB128ToI64 cntent
getGlobals _ = throw $ WasmError "getGlobals: bad section"
