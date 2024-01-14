{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Parser
-}

module Parsing.Sections
(
  getSections,
  getSectionWithId
)
where

import qualified Data.ByteString.Lazy as BSL (ByteString, length, unpack, take, drop, splitAt)
import Control.Exception (throw)

import Types (FileContent, Section(..), SectionID(..))
import Errors (CustomException(..))
import Leb128 (getLEB128ToI64)

extractHeader :: BSL.ByteString -> (Section, BSL.ByteString)
extractHeader bytes
  | (BSL.length bytes) < 8 = throw (WasmError "Invalid header")
  | otherwise = (Section CustomID 8 (BSL.take 8 bytes), BSL.drop 8 bytes)

getSectionId :: BSL.ByteString -> SectionID
getSectionId bytes = case head (BSL.unpack $ BSL.take 1 bytes) of
  0 -> CustomID
  1 -> TypeID
  2 -> ImportID
  3 -> FunctionID
  4 -> TableID
  5 -> MemoryID
  6 -> GlobalID
  7 -> ExportID
  8 -> StartID
  9 -> ElementID
  10 -> CodeID
  11 -> DataID
  _ -> throw (WasmError "Invalid section id")

extractSection :: BSL.ByteString -> (Section, BSL.ByteString)
extractSection bytes = (Section sectionId (fromIntegral size) content, rest2)
  where
    sectionId = getSectionId bytes
    (size, rest) = getLEB128ToI64 (BSL.drop 1 bytes)
    (content, rest2) = BSL.splitAt size rest

extractSections :: BSL.ByteString -> [Section]
extractSections bytes
  | BSL.length bytes == 0 = []
  | otherwise = section : extractSections rest
  where
    (section, rest) = extractSection bytes

getSections :: FileContent -> [Section]
getSections bytes = header : sections
  where
    (header, rest) = extractHeader bytes
    sections = extractSections rest

getSectionWithId :: [Section] -> SectionID -> Section
getSectionWithId [] _ = throw (WasmError "No section with this id")
getSectionWithId (x:xs) id
  | identifier x == id = x
  | otherwise = getSectionWithId xs id