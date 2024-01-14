{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Leb128
-}

module Leb128
(
  getLEB128,
  extractLEB128,
  extractLEB1282,
)
where

import Data.Binary.Get
import Data.Bits
import Data.Int (Int64, Int32)
import qualified Data.ByteString.Lazy as BS (ByteString, drop)

getLEB128 :: Get Int
getLEB128 = do
  byte <- getWord8
  let value = fromIntegral (byte .&. 0x7F)
  if byte `testBit` 7
    then do
      next <- getLEB128
      return $ value .|. (next `shiftL` 7)
    else
      return value

extractLEB128' :: Get (Int64, Int64)
extractLEB128' = do
  byte <- getWord8
  let value = fromIntegral (byte .&. 0x7F)
  case byte `testBit` 7 of
    True -> do
      (next, size) <- extractLEB128'
      return (value .|. (next `shiftL` 7), size + 1)
    False -> return (value, 1)

--function that returns the value and the rest of the bytestring
extractLEB128 :: BS.ByteString -> (Int64, BS.ByteString)
extractLEB128 bytes = do
  let (value, size) = runGet extractLEB128' bytes
  (value, BS.drop size bytes)

extractLEB1282' :: Get (Int32, Int64)
extractLEB1282' = do
  byte <- getWord8
  let value = fromIntegral (byte .&. 0x7F)
  case byte `testBit` 7 of
    True -> do
      (next, size) <- extractLEB1282'
      return (value .|. (next `shiftL` 7), size + 1)
    False -> return (value, 1)

extractLEB1282 :: BS.ByteString -> (Int32, BS.ByteString)
extractLEB1282 bytes = do
  let (value, size) = runGet extractLEB1282' bytes
  (value, BS.drop size bytes)
