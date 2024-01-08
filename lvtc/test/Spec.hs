{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Tests
-}

import Test.Tasty
import Test.Tasty.HUnit

import Expression
import Parser
import Alias

import UTParseLvt
import UTShuntingYard
import UTWatLike

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Leviator Tests - Compiler"
    [
        utParserExpression,
        utParserExpressions,
        utParserLvt,
        utShuntingYard,
        utAlias,
        utWatLike
    ]

testParserHelper :: String -> String -> Expression -> IO ()
testParserHelper str restExpected expressionExpected =
    case runParser parseExpresion str of
        Just (parsed, rest) -> assertEqual str restExpected rest >>
            assertEqual str expressionExpected parsed
        Nothing -> assertFailure ("Parsing failed for: `" ++ str ++ "`")

testParserHelperFail :: String -> IO ()
testParserHelperFail str = case runParser parseExpresion str of
    Just _ -> assertFailure ("Parsing should have failed: " ++ str)
    Nothing -> assertEqual str "" ""

utParserExpression :: TestTree
utParserExpression = testGroup "Parse Expression"
  [
-- function
    testCase "function main" $
      testParserHelper
        "fn main() -> Int \n{\n    <- 0;\n};\n"
        ""
        (Function "fn main() -> Int \n{\n    <- 0;\n};\n")
  , testCase "function bad formated (no end `}`)" $
      testParserHelperFail
        "fn main() -> Int \n{\n    <- 0;\n"
  , testCase "function bad formated (no end `;`)" $
      testParserHelperFail
        "fn main() -> Int \n{\n    <- 0;\n}\n"
  , testCase "function bad formated (no end `\\n`)" $
      testParserHelperFail
        "fn main() -> Int \n{\n    <- 0;\n};"
  , testCase "function export" $
      testParserHelper
        "export fn main() -> Int \n{\n    <- 0;\n};\n"
        ""
        (Function "export fn main() -> Int \n{\n    <- 0;\n};\n")
-- alias
  , testCase "alias" $
      testParserHelper
        "alias abc def;\n"
        ""
        (Expression.Alias "alias abc def;\n")
  , testCase "alias bad formated (no end `\\n`)" $
      testParserHelperFail
        "alias abc def;"
  , testCase "alias bad formated (no end `;`)" $
      testParserHelperFail
        "alias abc def\n"
-- comment
  , testCase "comment" $
      testParserHelper
        "// this is a comment\n"
        ""
        (Comment "// this is a comment\n")
  , testCase "comment bad formated (no end `\\n`)" $
      testParserHelperFail
        "// this is a comment"
-- bad formated
  , testCase "bad formated" $
      testParserHelperFail
        "abc"
  , testCase "bad formated 2" $
      testParserHelperFail
        "/ def;\n"
  , testCase "bad formated 3" $
      testParserHelperFail
        "def;\n"
  , testCase "bad formated 4" $
      testParserHelperFail
        "export abc()"
  ]


testParserHelpers :: String -> String -> [Expression] -> IO ()
testParserHelpers str restExpected expressionExpected =
    case runParser parseAllExpression str of
        Just (parsed, rest) -> assertEqual str restExpected rest >>
            assertEqual str expressionExpected parsed
        Nothing -> assertFailure ("Parsing failed: " ++ str)

testParserHelperFails :: String -> IO ()
testParserHelperFails str = case runParser parseAllExpression str of
    Just (x, _) -> assertFailure ("Parsing should have failed: `" ++ str ++ "` But got: `" ++ show x ++ "`")
    Nothing -> assertEqual str "" ""

utParserExpressions :: TestTree
utParserExpressions = testGroup "Parse Expressions"
  [
-- function
    testCase "function main" $
      testParserHelpers
        "fn main() -> Int \n{\n    <- 0;\n};\nexport fn main() -> Int \n{\n    <- 0;\n};\n"
        ""
        [Function "fn main() -> Int \n{\n    <- 0;\n};\n", Function "export fn main() -> Int \n{\n    <- 0;\n};\n"]
  , testCase "function bad formated (no end `}`)" $
      testParserHelperFails
        "fn main() -> Int \n{\n    <- 0;\n};\nfn main() -> Int \n{\n    <- 0;\n"
  , testCase "function bad formated (no end `;`)" $
      testParserHelperFails
        "fn main() -> Int \n{\n    <- 0;\n}\nfn main() -> Int \n{\n    <- 0;\n};\n"
-- alias
  , testCase "alias" $
      testParserHelpers
        "alias abc def;\nalias def def;\n"
        ""
        [Expression.Alias "alias abc def;\n", Expression.Alias "alias def def;\n"]
  , testCase "alias multiline" $
      testParserHelpers
        "alias abc def\nefg hij;\n"
        ""
        [Expression.Alias "alias abc def\nefg hij;\n"]
-- comment
  , testCase "comment" $
      testParserHelpers
        "// this is a comment\nalias abc def;\n"
        ""
        [Comment "// this is a comment\n", Expression.Alias "alias abc def;\n"]
  ]

utAlias :: TestTree
utAlias = testGroup "Alias"
  [
    testCase "alias" $
      assertEqual "alias"
        [
            Expression.Function "fn main() -> Int \n{\n    <- 0;\n};"
        ]
        (proceedAlias [
            Expression.Alias "alias int Int;\n",
            Expression.Alias "alias retValue 0;\n",
            Expression.Function "fn main() -> int \n{\n    <- retValue;\n};"
        ])
  , testCase "nested alias" $
      assertEqual "alias nested"
        [
            Expression.Function "fn main() -> Int \n{\n    <- 0;\n};"
        ]
        (proceedAlias [
            Expression.Alias "alias int INT;\n",
            Expression.Alias "alias retValue 0;\n",
            Expression.Alias "alias INT Int;\n",
            Expression.Function "fn main() -> int \n{\n    <- retValue;\n};"
        ])
  ]
