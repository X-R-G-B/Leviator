{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- OpCodes
-}

module OpCodes
(
  extractOpCode,
  createInstruction
)
where

import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throw)
import Data.Word (Word8)

import Leb128
import Types
import Errors

extractOpCode :: BSL.ByteString -> ([Word8], BSL.ByteString)
extractOpCode bytes = case BSL.unpack bytes of
  0x03 : rest -> ([0x03], BSL.pack rest)
  0x11 : rest -> ([0x11], BSL.pack rest)
  0x00 : rest -> ([0x00], BSL.pack rest)
  0x0b : rest -> ([0x0b], BSL.pack rest)
  0x0d : rest -> ([0x0d], BSL.pack rest)
  0x0c : rest -> ([0x0c], BSL.pack rest)
  0x02 : rest -> ([0x02], BSL.pack rest)
  0x01 : rest -> ([0x01], BSL.pack rest)
  0x0f : rest -> ([0x0f], BSL.pack rest)
  0x10 : rest -> ([0x10], BSL.pack rest)
  0x41 : rest -> ([0x41], BSL.pack rest)
  0x42 : rest -> ([0x42], BSL.pack rest)
  0x6c : rest -> ([0x6c], BSL.pack rest)
  0x6d : rest -> ([0x6d], BSL.pack rest)
  0x43 : rest -> ([0x43], BSL.pack rest)
  0x44 : rest -> ([0x44], BSL.pack rest)
  0x28 : rest -> ([0x28], BSL.pack rest)
  0x29 : rest -> ([0x29], BSL.pack rest)
  0x22 : rest -> ([0x22], BSL.pack rest)
  0x36 : rest -> ([0x36], BSL.pack rest)
  0x37 : rest -> ([0x37], BSL.pack rest)
  0x4b : rest -> ([0x4b], BSL.pack rest)
  0x20 : rest -> ([0x20], BSL.pack rest)
  0x4d : rest -> ([0x4d], BSL.pack rest)
  0x21 : rest -> ([0x21], BSL.pack rest)
  0x23 : rest -> ([0x23], BSL.pack rest)
  0x24 : rest -> ([0x24], BSL.pack rest)
  0x6a : rest -> ([0x6a], BSL.pack rest)
  0x6b : rest -> ([0x6b], BSL.pack rest)
  0x45 : rest -> ([0x45], BSL.pack rest)
  0x46 : rest -> ([0x46], BSL.pack rest)
  0x71 : rest -> ([0x00], BSL.pack rest)
  0x48 : rest -> ([0x48], BSL.pack rest)
  0x4a : rest -> ([0x4a], BSL.pack rest)
  0x4c : rest -> ([0x4c], BSL.pack rest)
  0x4e : rest -> ([0x4e], BSL.pack rest)
  0x47 : rest -> ([0x47], BSL.pack rest)
  0x3f : 0x00 : rest -> ([0x3f, 0x00], BSL.pack rest)
  0x40 : 0x00 : rest -> ([0x40, 0x00], BSL.pack rest)
  0x04 : 0x40 : rest -> ([0x04, 0x40], BSL.pack rest)
  _ -> throw $ WasmError "ExtractOpCode: bad opcode"

createInstruction :: [Word8] -> BSL.ByteString -> (Instruction, BSL.ByteString)
createInstruction [0x03] bytes = (Nop, bytes)
createInstruction [0x11] bytes = (Nop, bytes)
createInstruction [0x00] bytes = (Unreachable, bytes)
createInstruction [0x01] bytes = (Nop, bytes)
createInstruction [0x02] bytes = (Block EmptyType, bytes)
createInstruction [0x0b] bytes = (End, bytes)
createInstruction [0x48] bytes = (I32Lts, bytes)
createInstruction [0x0f] bytes = (Return, bytes)
createInstruction [0x4b] bytes = (I32Gtu, bytes)
createInstruction [0x6a] bytes = (I32Add, bytes)
createInstruction [0x6c] bytes = (I32Mul, bytes)
createInstruction [0x6d] bytes = (I32Divs, bytes)
createInstruction [0x47] bytes = (I32Ne, bytes)
createInstruction [0x6b] bytes = (I32Sub, bytes)
createInstruction [0x4a] bytes = (I32Gts, bytes)
createInstruction [0x46] bytes = (I32Eq, bytes)
createInstruction [0x45] bytes = (I32Eqz, bytes)
createInstruction [0x4d] bytes = (I32Leu, bytes)
createInstruction [0x4e] bytes = (I32Ges, bytes)
createInstruction [0x4c] bytes = (I32Les, bytes)
createInstruction [0x71] bytes = (I32And, bytes)
createInstruction [0x04, 0x40] bytes = (If, bytes)
createInstruction [0x3f, 0x00] bytes = (MemorySize, bytes)
createInstruction [0x40, 0x00] bytes = (MemoryGrow, bytes)
createInstruction [0x0d] bytes = (\(value, rest) -> (BrIf value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x0c] bytes = (\(value, rest) -> (Br value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x22] bytes = (\(value, rest) -> (LocalTee value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x10] bytes = (\(value, rest) -> (Call value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x41] bytes = (\(value, rest) -> (I32Const value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x42] bytes = (\(value, rest) -> (I64Const value, rest)) (getLEB128ToI64 bytes)
createInstruction [0x43] bytes = (\(value, rest) -> (F32Const (fromIntegral value), rest)) (getLEB128ToI32 bytes)
createInstruction [0x20] bytes = (\(value, rest) -> (GetLocal value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x24] bytes = (\(value, rest) -> (SetGlobal value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x23] bytes = (\(value, rest) -> (GetGlobal value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x21] bytes = (\(value, rest) -> (SetLocal value, rest)) (getLEB128ToI32 bytes)
createInstruction [0x44] bytes = (\(value, rest) -> (F64Const (fromIntegral value), rest)) (getLEB128ToI64 bytes)
createInstruction [0x28] bytes = (\(align, rest) -> (\(offset, rest2) -> (I32Load (MemArg offset align), rest2)) (getLEB128ToI32 rest)) (getLEB128ToI32 bytes)
createInstruction [0x29] bytes = (\(align, rest) -> (\(offset, rest2) -> (I64Load (MemArg offset align), rest2)) (getLEB128ToI32 rest)) (getLEB128ToI32 bytes)
createInstruction [0x36] bytes = (\(align, rest) -> (\(offset, rest2) -> (I32Store (MemArg offset align), rest2)) (getLEB128ToI32 rest)) (getLEB128ToI32 bytes)
createInstruction [0x37] bytes = (\(align, rest) -> (\(offset, rest2) -> (I64Store (MemArg offset align), rest2)) (getLEB128ToI32 rest)) (getLEB128ToI32 bytes)
createInstruction _ _ = throw $ WasmError "createInstruction: bad instruction"
