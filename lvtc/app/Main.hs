{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Main
-}

module Main (main) where

import Expression (parseExpresion, parseAllExpression)
import Parser (runParser)
import Alias (proceedAlias)
import ParseLvt (parseInstruction, parseInstructions, parseFuncDeclaration)
import WatLike (aSTToWatLike)
import AST
import WatAST
import Builtins (getBuiltinsWat)
import WatToWasm (watToWasm)

test1 :: String
test1 = "if (a)\n{\nb(0);\n};\n"

test2 :: String
test2 = part1 ++ part2
    where
        part1 = "@Int a = 0;\n    @Int c = b(a);\n"
        part2 = "    if (c)\n    {\n        d(a);\n    };\n"

test3 :: String
test3 = "alias abc def;\n"

test4 :: String
test4 = "// this is a comment\n"

test5 :: String
test5 = "@Int a = 4 + 5;\n"

test6 :: String
test6 = "@Int a = 3 + 4 * 2 / ( 1 - 5 );\n"

text :: String
text = aliasInt ++ aliasRetValue ++ funcMain
    where
        aliasInt = "alias int Int;\n"
        aliasRetValue = "alias retValue 0;\n"
        funcMain = "fn main () -> int \n{\n    <- retValue;\n};\n"

test8 :: String
test8 = "fn abc(a: Int) -> Int\n{\n    <- a;\n};\n"

test7 :: [FuncDeclaration]
test7 =
    [
        (
            (False, "add", [("a", "Int"), ("b", "Int")], "Int"),
            [AST.Return (FuncValue ("+", [Var "a", Var "b"]))]
        )
    ]

test9 :: [FuncDef]
test9 =
    getBuiltinsWat ++
    [
        FuncDef False "add" 10 [] WatAST.I32 [
            I32Const 97,
            LocalSet 0,
            LocalGet 0,
            WatAST.Return
        ] [(WatAST.I32, 1)]
    ]

main2 :: IO ()
main2 =
    print (aSTToWatLike test7)
    >> print (watToWasm test9)

main :: IO ()
main =
    print (runParser parseInstruction test1)
    >> print (runParser parseInstructions test2)
    >> print (runParser parseExpresion test3)
    >> print (runParser parseExpresion test4)
    >> print (runParser parseInstruction test5)
    >> print (runParser parseInstruction test6)
    >> print (runParser (proceedAlias <$> parseAllExpression) text)
    >> print (runParser parseFuncDeclaration test8)
    >> main2
