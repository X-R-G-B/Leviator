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

import Data.Word (Word8)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import Leb128 (getLEB128ToI64)
import Errors (CustomException(..))
import Types (FileContent, Section(..), SectionID(..))

extractHeader :: BSL.ByteString -> (Section, BSL.ByteString)
extractHeader bytes
  | (BSL.length bytes) < 8 = throw (WasmError "Invalid header")
  | otherwise = (Section CustomID 8 (BSL.take 8 bytes), BSL.drop 8 bytes)

getSectionId' :: Word8 -> SectionID
getSectionId' 0 = CustomID
getSectionId' 1 = TypeID
getSectionId' 2 = ImportID
getSectionId' 3 = FunctionID
getSectionId' 4 = TableID
getSectionId' 5 = MemoryID
getSectionId' 6 = GlobalID
getSectionId' 7 = ExportID
getSectionId' 8 = StartID
getSectionId' 9 = ElementID
getSectionId' 10 = CodeID
getSectionId' 11 = DataID
getSectionId' _ = throw (WasmError "Invalid section id")

getSectionId :: BSL.ByteString -> SectionID
getSectionId bytes = getSectionId' (head (BSL.unpack bytes))

extractSection :: BSL.ByteString -> (Section, BSL.ByteString)
extractSection bytes = (Section sectionId (fromIntegral sze) cntent, rest2)
  where
    sectionId = getSectionId bytes
    (sze, rest) = getLEB128ToI64 (BSL.drop 1 bytes)
    (cntent, rest2) = BSL.splitAt sze rest

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
getSectionWithId (x:xs) idtfier
  | identifier x == idtfier = x
  | otherwise = getSectionWithId xs idtfier
