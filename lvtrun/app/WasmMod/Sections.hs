{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Sections
-}

module WasmMod.Sections
  (
    SectionID(..),
    Section(..)
  )
where

import qualified Data.ByteString as BS (ByteString)

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
  id :: SectionID,
  size :: Int,
  content :: BS.ByteString
} deriving (Show)
