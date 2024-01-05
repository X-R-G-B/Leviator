{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Header
-}

module WasmMod.Header
  (
    ModHeader(..),
    getModuleHeader,
    isHeaderValid
  )
where

import qualified Data.ByteString as BS (ByteString, take, drop, pack)

data ModHeader = ModHeader {
  magicNumber :: BS.ByteString,
  version :: BS.ByteString
} deriving (Show)

getModuleHeader :: BS.ByteString -> ModHeader
getModuleHeader bytes = ModHeader (BS.take 4 bytes) (BS.take 4 $ BS.drop 4 bytes)

isHeaderValid :: ModHeader -> Bool
isHeaderValid header =
  magicNumber header == BS.pack [0x00, 0x61, 0x73, 0x6d] &&
    version header == BS.pack [0x01, 0x00, 0x00, 0x00]
