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

import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throw)
import Data.Word (Word8)
import Data.Int (Int64)

import Leb128
import Types
import Errors
import OpCodes

parseInstruction :: BSL.ByteString -> (Instruction, BSL.ByteString)
parseInstruction bytes
  | BSL.length bytes == 0 = throw $ WasmError "ParseInstruction: no instruction"
  | otherwise = do
    let (opCode, rest) = extractOpCode bytes
    let (instruction, rest2) = createInstruction opCode rest
    (instruction, rest2)

parseInstructions :: BSL.ByteString -> [Instruction]
parseInstructions bytes
  | BSL.length bytes == 0 = []
  | head (BSL.unpack bytes) == 0x0b = []
  | otherwise = do
    let (instruction, rest) = parseInstruction bytes
    instruction : parseInstructions rest

parseMutability :: Word8 -> Mutability
parseMutability 0x00 = Const
parseMutability 0x01 = Var
parseMutability _ = throw $ WasmError "ParseMutability: bad mutability"

getHexaIndex :: BSL.ByteString -> Int64 -> Int64
getHexaIndex content idx
  | idx >= (fromIntegral $ BSL.length content) = throw $ WasmError "GetHexaIndex: no 0x0b found"
  | (head $ BSL.unpack $ BSL.drop (fromIntegral idx) content) == 0x0b = idx
  | otherwise = getHexaIndex content (idx + 1)

extractExpression :: BSL.ByteString -> (BSL.ByteString, BSL.ByteString)
extractExpression content = do
    let idx = getHexaIndex content 0
    let expression = BSL.take (fromIntegral (idx + 1)) content
    let rest = BSL.drop (fromIntegral (idx + 1)) content
    (expression, rest)

parseGlobal :: BSL.ByteString -> (Global, BSL.ByteString)
parseGlobal content = do
    let globalType = getTypeFromByte (head $ BSL.unpack content)
    let mutability = parseMutability (head $ BSL.unpack $ BSL.drop 1 content)
    let (expression, rest) = extractExpression (BSL.drop 2 content)
    let instructions = parseInstructions expression
    (Global globalType mutability instructions, rest)

parseGlobals :: Int64 -> Int64 -> BSL.ByteString -> [Global]
parseGlobals idx maxIdx content
  | idx >= maxIdx = []
  | otherwise = do
    let (global, rest) = parseGlobal content
    global : parseGlobals (idx + 1) maxIdx rest

getGlobals :: Section -> [Global]
getGlobals (Section GlobalID _ content) = do
  let (vecSize, rest) = getLEB128ToI64 content
  parseGlobals 0 vecSize rest
getGlobals _ = throw $ WasmError "getGlobals: bad section"
