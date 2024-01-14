{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ParseLvt
-}

module Lexeme
(
    lexeme1
) where

replaceN :: Int -> String -> String
replaceN _ [] = []
replaceN 0 ('"':xs) = '"' : replaceN 1 xs
replaceN 1 ('\\':'n':xs) = '\n' : replaceN 1 xs
replaceN 1 ('\\':'t':xs) = '\t' : replaceN 1 xs
replaceN 1 ('\\':'v':xs) = '\v' : replaceN 1 xs
replaceN 1 ('\\':'a':xs) = '\a' : replaceN 1 xs
replaceN 1 ('\\':'f':xs) = '\f' : replaceN 1 xs
replaceN 1 ('\\':'r':xs) = '\r' : replaceN 1 xs
replaceN 1 ('\\':x:xs) = '\\':x : replaceN 1 xs
replaceN 1 ('"':xs) = '"' : replaceN 0 xs
replaceN 0 ('\n':xs) = ' ' : replaceN 0 xs
replaceN n (x:xs) = x : replaceN n xs

lexeme :: Int -> String -> String
lexeme _ [] = []
lexeme 0 (' ':')':xs) = lexeme 0 (')':xs)
lexeme 0 (' ':'(':xs) = lexeme 0 ('(':xs)
lexeme 0 (' ':'}':xs) = lexeme 0 ('}':xs)
lexeme 0 (' ':'{':xs) = lexeme 0 ('{':xs)
lexeme 0 (' ':']':xs) = lexeme 0 (']':xs)
lexeme 0 (' ':'[':xs) = lexeme 0 ('[':xs)
lexeme 0 (' ':'+':xs) = lexeme 0 ('+':xs)
lexeme 0 (' ':'-':xs) = lexeme 0 ('-':xs)
lexeme 0 (' ':'*':xs) = lexeme 0 ('*':xs)
lexeme 0 (' ':'/':xs) = lexeme 0 ('/':xs)
lexeme 0 (' ':'<':xs) = lexeme 0 ('<':xs)
lexeme 0 (' ':'>':xs) = lexeme 0 ('>':xs)
lexeme 0 (' ':'=':xs) = lexeme 0 ('=':xs)
lexeme 0 (' ':'!':xs) = lexeme 0 ('!':xs)
lexeme 0 (' ':':':xs) = lexeme 0 (':':xs)
lexeme 0 (' ':',':xs) = lexeme 0 (',':xs)
lexeme 0 (' ':'@':xs) = lexeme 0 ('@':xs)
lexeme 0 (' ':';':xs) = lexeme 0 (';':xs)
lexeme 0 ('(':' ':xs) = lexeme 0 ('(':xs)
lexeme 0 (')':' ':xs) = lexeme 0 (')':xs)
lexeme 0 ('}':' ':xs) = lexeme 0 ('}':xs)
lexeme 0 ('{':' ':xs) = lexeme 0 ('{':xs)
lexeme 0 (']':' ':xs) = lexeme 0 (']':xs)
lexeme 0 ('[':' ':xs) = lexeme 0 ('[':xs)
lexeme 0 ('+':' ':xs) = lexeme 0 ('+':xs)
lexeme 0 ('-':' ':xs) = lexeme 0 ('-':xs)
lexeme 0 ('*':' ':xs) = lexeme 0 ('*':xs)
lexeme 0 ('/':' ':xs) = lexeme 0 ('/':xs)
lexeme 0 ('<':' ':xs) = lexeme 0 ('<':xs)
lexeme 0 ('>':' ':xs) = lexeme 0 ('>':xs)
lexeme 0 ('=':' ':xs) = lexeme 0 ('=':xs)
lexeme 0 ('!':' ':xs) = lexeme 0 ('!':xs)
lexeme 0 (':':' ':xs) = lexeme 0 (':':xs)
lexeme 0 (',':' ':xs) = lexeme 0 (',':xs)
lexeme 0 ('@':' ':xs) = lexeme 0 ('@':xs)
lexeme 0 (';':' ':xs) = lexeme 0 (';':xs)
lexeme 0 ('i':'f':' ':xs) = lexeme 0 ('i':'f':xs)
lexeme 0 (' ':'i':'f':xs) = lexeme 0 ('i':'f':xs)
lexeme 0 ('e':'l':'s':'e':' ':xs) = lexeme 0 ('e':'l':'s':'e':xs)
lexeme 0 (' ':'e':'l':'s':'e':xs) = lexeme 0 ('e':'l':'s':'e':xs)
lexeme 0 ('\\':x:xs) = x : lexeme 0 xs
lexeme 1 ('\\':x:xs) = x : lexeme 1 xs
lexeme 0 (' ':' ':xs) = lexeme 0 (' ':xs)
lexeme 1 ('"':xs) = '"' : lexeme 0 xs
lexeme 0 ('"':xs) = '"' : lexeme 1 xs
lexeme n (x:xs) = x : lexeme n xs

stripLastSpaces :: String -> String
stripLastSpaces = reverse . dropWhile (== ' ') . reverse

lexeme1 :: String -> String
lexeme1 str = stripLastSpaces $ lexeme 0 (replaceN 0 str)
