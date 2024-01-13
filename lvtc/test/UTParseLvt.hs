{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- UTParseLvt
-}

module UTParseLvt (
    utParserLvt
) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import ParseLvt
import AST

testParserHelper :: String -> String -> Instruction -> IO ()
testParserHelper str restExpected expressionExpected =
    case runParser parseInstruction str of
        Just (parsed, rest) -> assertEqual str restExpected rest >>
            assertEqual str expressionExpected parsed
        Nothing -> assertFailure ("Parsing failed for: `" ++ str ++ "`")

testParserFunc :: String -> String -> FuncDeclaration -> IO ()
testParserFunc str restExpected expressionExpected =
    case runParser parseFuncDeclaration str of
        Just (parsed, rest) -> assertEqual str restExpected rest >>
            assertEqual str expressionExpected parsed
        Nothing -> assertFailure ("Parsing failed for: `" ++ str ++ "`")

testParserHelpers :: String -> String -> [Instruction] -> IO ()
testParserHelpers str restExpected expressionExpected =
    case runParser parseInstructions str of
        Just (parsed, rest) -> assertEqual str restExpected rest >>
            assertEqual str expressionExpected parsed
        Nothing -> assertFailure ("Parsing failed for: `" ++ str ++ "`")

utParserLvt :: TestTree
utParserLvt = testGroup "Parse Lvt"
  [
    testCase "declare int" $
      testParserHelper "@Int a = 0;\n"
        ""
        (Declaration (("a", "Int"), Integer 0))
  , testCase "declare bool" $
      testParserHelper "@Bool a = True;\n"
        ""
        (Declaration (("a", "Bool"), Boolean True))
  , testCase "declare string view" $
      testParserHelper "@StringView a = \"abc\";\n"
        ""
        (Declaration (("a", "StringView"), StringView "abc"))
  , testCase "declare character" $
      testParserHelper "@Char a = 'a';\n"
        ""
        (Declaration (("a", "Char"), Character 'a'))
  , testCase "assign variable" $
      testParserHelper "a = 0;\n"
        ""
        (Assignation ("a", Integer 0))
  , testCase "call function" $
      testParserHelper "a(0);\n"
        ""
        (Function ("a", [Integer 0]))
  , testCase "call function (no arguments)" $
      testParserHelper "a();\n"
        ""
        (Function ("a", []))
  , testCase "call function (3 arguments)" $
      testParserHelper "a(0, \"abc\", False);\n"
        ""
        (Function ("a", [Integer 0, StringView "abc", Boolean False]))
  , testCase "return value" $
      testParserHelpers "<- 0;\n"
        ""
        [(Return (Integer 0))]
  , testCase "condition if" $
      testParserHelper "if (a)\n{\nb(0);\n};\n"
        ""
        (Cond (Var "a", [Function ("b", [Integer 0])], []))
  , testCase "condition if else" $
      testParserHelper "if (a)\n{\nb(0);\n}\nelse\n{\nc(0);\n};\n"
        ""
        (Cond (Var "a", [Function ("b", [Integer 0])], [Function ("c", [Integer 0])]))
  , testCase "condition if with indent" $
      testParserHelper "if (a)\n{\n    b(0);\n};\n"
        ""
        (Cond (Var "a", [Function ("b", [Integer 0])], []))
  , testCase "condition if else with indent" $
      testParserHelper "if (a)\n{\n    b(0);\n}\nelse\n{\n    c(0);\n};\n"
        ""
        (Cond (Var "a", [Function ("b", [Integer 0])], [Function ("c", [Integer 0])]))
  , testCase "test multiple instruction" $
      testParserHelpers "@Int a = 0;\n    @Int c = b(a);\n    if (c)\n    {\n        d(a);\n    };\n"
        ""
        [
            Declaration (("a", "Int"), Integer 0),
            Declaration (("c", "Int"), FuncValue ("b", [Var "a"])),
            Cond (Var "c", [Function ("d", [Var "a"])], [])
        ]
  , testCase "test func" $
      testParserFunc "fn abc(a: Int) -> Int\n{\n    <- a;\n};\n"
        ""
        (
            (False, "abc", [("a", "Int")], "Int"),
            [
                Return (Var "a")
            ]
        )
  , testCase "test func export" $
      testParserFunc "export fn abc(a: Int) -> Int\n{\n    <- a;\n};\n"
        ""
        (
            (True, "abc", [("a", "Int")], "Int"),
            [
                Return (Var "a")
            ]
        )
  , testCase "test func start" $
      testParserFunc "export fn start() -> Int\n{\n    <- 0;\n};\n"
        ""
        (
            (True, "start", [], "Int"),
            [
                Return (Integer 0)
            ]
        )
  ]
