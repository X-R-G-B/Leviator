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
getHexaIndex content idx
  | idx >= (fromIntegral $ BSL.length content) =
    throw $ WasmError "GetHexaIndex: no 0x0b found"
  | (head $ BSL.unpack $ BSL.drop (fromIntegral idx) content) == 0x0b = idx
  | otherwise = getHexaIndex content (idx + 1)

extractExpression :: BSL.ByteString -> (BSL.ByteString, BSL.ByteString)
extractExpression content = (expression, rest)
  where
    idx = getHexaIndex content 0
    expression = BSL.take (fromIntegral (idx + 1)) content
    rest = BSL.drop (fromIntegral (idx + 1)) content

parseGlobal :: BSL.ByteString -> (Global, BSL.ByteString)
parseGlobal content = (Global globalType mutability instructions, rest)
  where
    globalType = getTypeFromByte (head $ BSL.unpack content)
    mutability = parseMutability (head $ BSL.unpack $ BSL.drop 1 content)
    (expression, rest) = extractExpression (BSL.drop 2 content)
    instructions = parseInstructions expression

parseGlobals :: Int64 -> Int64 -> BSL.ByteString -> [Global]
parseGlobals idx maxIdx content
  | idx >= maxIdx = []
  | otherwise = global : parseGlobals (idx + 1) maxIdx rest
  where
    (global, rest) = parseGlobal content

getGlobals :: Section -> [Global]
getGlobals (Section GlobalID _ content) =
  parseGlobals 0 vecSize rest
  where
    (vecSize, rest) = getLEB128ToI64 content
getGlobals _ = throw $ WasmError "getGlobals: bad section"
