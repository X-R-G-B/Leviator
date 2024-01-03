
module UTParseLvt (
    utParserLvt
) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import ParseLvt

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
      testParserHelper "@Int a = 0;"
        ";"
        (Declare "Int" "a" (Integer 0))
  , testCase "declare bool" $
      testParserHelper "@Bool a = True;"
        ";"
        (Declare "Bool" "a" (Boolean True))
  , testCase "declare string view" $
      testParserHelper "@StringView a = \"abc\";"
        ";"
        (Declare "StringView" "a" (StringView "abc"))
  , testCase "declare character" $
      testParserHelper "@Char a = 'a';"
        ";"
        (Declare "Char" "a" (Character 'a'))
  , testCase "assign variable" $
      testParserHelper "a = 0;"
        ";"
        (Assign "a" (Integer 0))
  , testCase "call function" $
      testParserHelper "a(0);"
        ";"
        (Call "a" [Integer 0])
  , testCase "call function (no arguments)" $
      testParserHelper "a();"
        ";"
        (Call "a" [])
  , testCase "call function (3 arguments)" $
      testParserHelper "a(0, \"abc\", False);"
        ";"
        (Call "a" [Integer 0, StringView "abc", Boolean False])
  , testCase "return value" $
      testParserHelper "<- 0;"
        ";"
        (Return (Integer 0))
  ]
