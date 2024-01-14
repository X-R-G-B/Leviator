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
import Control.Monad (when)
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
    (size, rest) = getLEB128ToI64 bytes
    (code, rest2) = BSL.splitAt size rest

createLocal :: LocalIdx -> TypeName -> Local
createLocal idx typee = Local {lcIdx = idx, lcType = typee}

extractLocal :: Int64 -> BSL.ByteString -> ([Local], BSL.ByteString)
extractLocal id bytes
  | BSL.length bytes == 0 = throw $ WasmError "extractLocal: bad section"
  | otherwise = (locals, BSL.drop 1 rest)
  where
    (nb, rest) = getLEB128ToI64 bytes
    typee = getTypeFromByte (head (BSL.unpack (BSL.take 1 rest)))
    locals = map (\x -> createLocal (fromIntegral id) typee) [0..nb - 1]

extractLocals :: Int64 -> Int64 -> BSL.ByteString -> ([Local], BSL.ByteString)
extractLocals id idMax bytes
  | id >= idMax = ([], bytes)
  | BSL.length bytes == 0 = ([], bytes)
  | otherwise = (local ++ locals, rest2)
  where
    (local, rest) = extractLocal id bytes
    (locals, rest2) = extractLocals (id + 1) idMax rest

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
getFuncCode (Section CodeID _ content) functions = do
  let (nbFunc, rest) = getLEB128ToI64 content
  let funcCodes = diviseBytes rest
  when (nbFunc /= fromIntegral (length funcCodes)) $
    throw $ WasmError "getFuncCode: bad section"
  parseFunctions funcCodes functions
getFuncCode _ _ = throw $ WasmError "getFuncCode: bad section"
