{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Leb128
-}

module Leb128
(
  getLEB128ToI64,
  getLEB128ToI32,
)
where

import Data.Binary.Get
import Data.Bits
import Data.Int (Int64, Int32)
import qualified Data.ByteString.Lazy as BS (ByteString, drop)

--------------------- TO INT64 ---------------------

getLEB128ToI64' :: Get (Int64, Int64)
getLEB128ToI64' = do
  byte <- getWord8
  let value = fromIntegral (byte .&. 0x7F)
  case byte `testBit` 7 of
    True -> do
      (next, size) <- getLEB128ToI64'
      return (value .|. (next `shiftL` 7), size + 1)
    False -> return (value, 1)

getLEB128ToI64 :: BS.ByteString -> (Int64, BS.ByteString)
getLEB128ToI64 bytes = do
  let (value, size) = runGet getLEB128ToI64' bytes
  (value, BS.drop size bytes)

--------------------- TO INT32 ---------------------

getLEB128ToI32' :: Get (Int32, Int64)
getLEB128ToI32' = do
  byte <- getWord8
  let value = fromIntegral (byte .&. 0x7F)
  case byte `testBit` 7 of
    True -> do
      (next, size) <- getLEB128ToI32'
      return (value .|. (next `shiftL` 7), size + 1)
    False -> return (value, 1)

getLEB128ToI32 :: BS.ByteString -> (Int32, BS.ByteString)
getLEB128ToI32 bytes = do
  let (value, size) = runGet getLEB128ToI32' bytes
  (value, BS.drop size bytes)
