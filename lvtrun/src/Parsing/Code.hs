{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Code
-}

module Parsing.Code
(
  getFuncCode,
)
where

import Data.Int (Int64)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import Types
import Leb128 (getLEB128ToI64)
import Errors (CustomException(..))
import OpCodes (extractOpCode, createInstruction)

diviseBytes :: BSL.ByteString -> [BSL.ByteString]
diviseBytes bytes
  | BSL.length bytes == 0 = []
  | otherwise = code : diviseBytes rest2
  where
    (sze, rest) = getLEB128ToI64 bytes
    (code, rest2) = BSL.splitAt sze rest

createLocal :: LocalIdx -> TypeName -> Local
createLocal idx typee = Local {lcIdx = idx, lcType = typee}

extractLocal :: Int64 -> BSL.ByteString -> ([Local], BSL.ByteString)
extractLocal idtf bytes
  | BSL.length bytes == 0 = throw $ WasmError "extractLocal: bad section"
  | otherwise = (lcals, BSL.drop 1 rest)
  where
    (nb, rest) = getLEB128ToI64 bytes
    typee = getTypeFromByte (head (BSL.unpack (BSL.take 1 rest)))
    lcals = map (\_ -> createLocal (fromIntegral idtf) typee) [0..nb - 1]

extractLocals :: Int64 -> Int64 -> BSL.ByteString -> ([Local], BSL.ByteString)
extractLocals idtf idMax bytes
  | idtf >= idMax = ([], bytes)
  | BSL.length bytes == 0 = ([], bytes)
  | otherwise = (local ++ lcals, rest2)
  where
    (local, rest) = extractLocal idtf bytes
    (lcals, rest2) = extractLocals (idtf + 1) idMax rest

-------------------------

parseInstruction :: BSL.ByteString -> (Instruction, BSL.ByteString)
parseInstruction bytes
  | BSL.length bytes == 0 =
    throw $ WasmError "ParseInstruction: no instruction"
  | otherwise = createInstruction opCode rest
  where
    (opCode, rest) = extractOpCode bytes

extractCode :: BSL.ByteString -> [Instruction]
extractCode bytes
  | BSL.length bytes == 0 = []
  | otherwise = instruction : extractCode rest
  where
    (instruction, rest) = parseInstruction bytes

parseFunction :: BSL.ByteString -> Function -> Function
parseFunction bytes func = func {locals = lcals, body = extractCode rest2}
  where
    (nbLocalsTypes, rest) = getLEB128ToI64 bytes
    (lcals, rest2) = extractLocals 0 nbLocalsTypes rest

parseFunctions :: [BSL.ByteString] -> [Function] -> [Function]
parseFunctions [] [] = []
parseFunctions [] _ = throw $ WasmError "parseFunctions: bad section"
parseFunctions _ [] = throw $ WasmError "parseFunctions: bad section"
parseFunctions (x:xs) (y:ys) = parseFunction x y : parseFunctions xs ys

getFuncCode :: Section -> [Function] -> [Function]
getFuncCode (Section CodeID _ cntent) fctns =
  parseFunctions funcCodes fctns
  where
    (_, rest) = getLEB128ToI64 cntent
    funcCodes = diviseBytes rest
getFuncCode _ _ = throw $ WasmError "getFuncCode: bad section"
