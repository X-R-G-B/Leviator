{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Main
-}

module Main (main) where

import Expression (parseAllExpression)
import Parser (runParser)
import Alias (proceedAlias)

text :: String
text = aliasInt ++ aliasRetValue ++ funcMain
    where
        aliasInt = "alias int Int;\n"
        aliasRetValue = "alias retValue 0;\n"
        funcMain = "fn main () -> int \n{\n    <- retValue;\n};\n"

main :: IO ()
main = print $ runParser (proceedAlias <$> parseAllExpression) text
