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
import Data.Word (Word8, Word64)
import Data.Int (Int32, Int64)
import Numeric (showHex)

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

getSectionSize :: BS.ByteString -> Int
getSectionSize bytes = runGet getLEB128 bytes

-- Returns the number of bytes used to encode the leb128
getLEB128Size' :: Get Int64
getLEB128Size' = do
  byte <- getWord8
  let value = fromIntegral (byte .&. 0x7F)
  if byte `testBit` 7
    then do
      next <- getLEB128Size'
      return (next + 1)
    else
      return 1

getLEB128Size :: BS.ByteString -> Int64
getLEB128Size bytes = runGet getLEB128Size' bytes

getSection :: BS.ByteString -> (Section, BS.ByteString)
getSection bytes = (Section id size content, rest)
  where
    id = getSectionID (BS.head bytes)
    nbByteEncoded = getLEB128Size (BS.drop 1 bytes)
    size = getSectionSize (BS.take (fromIntegral nbByteEncoded) (BS.drop 1 bytes))
    content = BS.take (fromIntegral size) (BS.drop (fromIntegral nbByteEncoded + 1) bytes)
    rest = BS.drop (nbByteEncoded + 1 + fromIntegral size) bytes

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
