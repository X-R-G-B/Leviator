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
import Data.Word (Word8)
import Numeric (showHex)
import WasmMod.Leb128
import Data.Bits

data SectionID =
  Custom
  | TypeID
  | ImportID
  | FunctionID
  | TableID
  | MemoryID
  | GlobalID
  | ExportID
  | StartID
  | ElementID
  | CodeID
  | DataID
  | DataCountID
  | InvalidID
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
getSectionID 1 = TypeID
getSectionID 2 = ImportID
getSectionID 3 = FunctionID
getSectionID 4 = TableID
getSectionID 5 = MemoryID
getSectionID 6 = GlobalID
getSectionID 7 = ExportID
getSectionID 8 = StartID
getSectionID 9 = ElementID
getSectionID 10 = CodeID
getSectionID 11 = DataID
getSectionID 12 = DataCountID
getSectionID _ = InvalidID

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

-- Todo: Check if sections are valid
getModSections :: BS.ByteString -> [Section]
getModSections bytes = getModSections' (removeHeader bytes)
