{-
-- EPITECH PROJECT, 2023
-- koaky
-- File description:
-- lib version
-}

module KoakyLibVersion
    (
        koakyLibVersionPatch,
        koakyLibVersionMinor,
        koakyLibVersionMajor,
        koakyLibVersion
    )
    where

koakyLibVersionPatch :: Int
koakyLibVersionPatch = 0

koakyLibVersionMinor :: Int
koakyLibVersionMinor = 0

koakyLibVersionMajor :: Int
koakyLibVersionMajor = 0


koakyLibVersion :: String
koakyLibVersion = fullVersion
    where
        fMaj = show koakyLibVersionMajor
        fMin = show koakyLibVersionMinor
        fPat = show koakyLibVersionPatch
        fullVersion = fMaj ++ "." ++ fMin ++ "." ++ fPat
