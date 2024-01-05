{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Sections
-}

module WasmMod.Sections
  (
    SectionID(..),
    Section(..),
    areSectionsValid,
    getModSections
  )
where

import qualified Data.ByteString.Lazy as BS (ByteString, head, drop, take, null, unpack)
import Data.Binary.Get
import Data.Bits
import Data.Int (Int32, Int64)
import Numeric (showHex)
import WasmMod.Leb128
import Data.Word
import WasmMod.Leb128

data SectionID =
  Custom
  | Type
  | Import
  | Function
  | Table
  | Memory
  | Global
  | Export
  | Start
  | Element
  | Code
  | Data
  | DataCount
  | Invalid
  deriving (Show, Eq)

data Section = Section {
  identifier :: SectionID,
  size :: Int,
  content :: BS.ByteString
}

instance Show Section where
  show section =
    "\nSection " ++ (show $ identifier section) ++
    " Size: " ++ (show $ size section) ++
    " Content: " ++ (concat $ map (\x -> showHex x " ") (BS.unpack $ content section))

areSectionsValid :: [Section] -> Bool
areSectionsValid sections = True

getSectionID :: Word8 -> SectionID
getSectionID 1 = Type
getSectionID 2 = Import
getSectionID 3 = Function
getSectionID 4 = Table
getSectionID 5 = Memory
getSectionID 6 = Global
getSectionID 7 = Export
getSectionID 8 = Start
getSectionID 9 = Element
getSectionID 10 = Code
getSectionID 11 = Data
getSectionID 12 = DataCount
getSectionID _ = Invalid

getSection :: BS.ByteString -> (Section, BS.ByteString)
getSection bytes = do
  let id = getSectionID (BS.head bytes)
  let (size, rest) = extractLEB128 (BS.drop 1 bytes)
  let content = BS.take (fromIntegral size) rest
  (Section id (fromIntegral size) content, BS.drop (fromIntegral size) rest)

removeHeader :: BS.ByteString -> BS.ByteString
removeHeader bytes = BS.drop 8 bytes

getModSections' :: BS.ByteString -> [Section]
getModSections' = do
  let getModSections'' bytes =
        if BS.null bytes
          then []
          else
            let (section, rest) = getSection bytes
            in section : getModSections'' rest
  getModSections''

getModSections :: BS.ByteString -> [Section]
getModSections bytes = getModSections' (removeHeader bytes)
