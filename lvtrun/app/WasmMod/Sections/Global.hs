{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Global
-}

module WasmMod.Sections.Global
  (
    parseGlobals,
    Global(..)
  )
where

import qualified Data.ByteString.Lazy as Bs
import Control.Exception (throw)
import Data.Word (Word8)
import Data.Int (Int64)

import WasmMod.Leb128
import WasmMod.Sections (Section(..), SectionID(..))
import Types (Type(..), getTypeFromByte)
import Errors

data Mutability = Const | Var deriving (Show)

data Global = Global {
    globalType :: Type,
    mutability :: Mutability,
    initExp :: [Word8]
}

instance Show Global where
  show global = "Global: " ++ (show $ globalType global) ++ " " ++
    (show $ mutability global) ++ " " ++ (show $ initExp global) ++ "\n"

parseMutability :: Word8 -> Mutability
parseMutability 0x00 = Const
parseMutability 0x01 = Var
parseMutability _ = throw $ WasmError "ParseMutability: bad mutability"

getHexaIndex :: Bs.ByteString -> Int64 -> Int64
getHexaIndex content idx
  | idx >= (fromIntegral $ Bs.length content) = throw $ WasmError "GetHexaIndex: no 0x0b found"
  | (head $ Bs.unpack $ Bs.drop (fromIntegral idx) content) == 0x0b = idx
  | otherwise = getHexaIndex content (idx + 1)

extractExpression :: Bs.ByteString -> ([Word8], Bs.ByteString)
extractExpression content = do
    let idx = getHexaIndex content 0
    let expression = Bs.take (fromIntegral (idx + 1)) content
    (Bs.unpack expression, Bs.drop (fromIntegral (idx + 1)) content)

parseGlobal :: Bs.ByteString -> (Global, Bs.ByteString)
parseGlobal content = do
    let globalType = getTypeFromByte (head $ Bs.unpack content)
    let mutability = parseMutability (head $ Bs.unpack $ Bs.drop 1 content)
    let (expression, rest) = extractExpression (Bs.drop 2 content)
    (Global globalType mutability expression, rest)

parseGlobals' :: Int64 -> Int64 -> Bs.ByteString -> [Global]
parseGlobals' idx maxIdx content
  | idx >= maxIdx = []
  | otherwise = do
    let (global, rest) = parseGlobal content
    global : parseGlobals' (idx + 1) maxIdx rest

parseGlobals :: Section -> [Global]
parseGlobals (Section GlobalID _ content) = do
  let (vecSize, rest) = extractLEB128 content
  parseGlobals' 0 vecSize rest
parseGlobals _ = throw $ WasmError "ParseGlobals: bad section"
