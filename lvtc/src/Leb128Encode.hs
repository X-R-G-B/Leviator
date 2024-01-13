{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Leb128Encode
-}

module Leb128Encode
(
    leb128Encode
) where

import Data.Bits

leb128Encode :: Int -> [Int]
leb128Encode n
    | n < 0x80 = [n]
    | otherwise = ((n .&. 0x7F) .|. 0x80) : leb128Encode (n `shiftR` 7)
