
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
      testParserHelper "<- 0;\n"
        ""
        (Return (Integer 0))
  ]
