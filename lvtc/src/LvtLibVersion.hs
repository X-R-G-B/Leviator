{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- LvtLibVersion
-}

module LvtLibVersion
    (
        lvtLibVersionPatch,
        lvtLibVersionMinor,
        lvtLibVersionMajor,
        lvtLibVersion
    )
    where

lvtLibVersionPatch :: Int
lvtLibVersionPatch = 0

lvtLibVersionMinor :: Int
lvtLibVersionMinor = 0

lvtLibVersionMajor :: Int
lvtLibVersionMajor = 0


lvtLibVersion :: String
lvtLibVersion = fullVersion
    where
        fMaj = show lvtLibVersionMajor
        fMin = show lvtLibVersionMinor
        fPat = show lvtLibVersionPatch
        fullVersion = fMaj ++ "." ++ fMin ++ "." ++ fPat
