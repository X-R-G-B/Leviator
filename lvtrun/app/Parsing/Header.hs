{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Header
-}

module Parsing.Header
  (
    getModHeader,
    isHeaderValid
  )
where

import qualified Data.ByteString.Lazy as BSL (ByteString, take, drop, pack)

import Types

getModHeader :: Section -> ModHeader
getModHeader bytes = ModHeader (BSL.take 4 $ content bytes) (BSL.take 4 $ BSL.drop 4 $ content bytes)

isHeaderValid :: ModHeader -> Bool
isHeaderValid header =
  magicNumber header == BSL.pack [0x00, 0x61, 0x73, 0x6d] &&
    version header == BSL.pack [0x01, 0x00, 0x00, 0x00]
