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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Leviator Tests - Compiler" [ utParserExpression ]

testParserHelper :: String -> String -> Expression -> IO ()
testParserHelper str restExpected expressionExpected =
    case runParser (parseExpresion) str of
        Just (parsed, rest) -> assertEqual str restExpected rest >>=
            (\_ -> assertEqual str expressionExpected parsed)
        Nothing -> assertFailure ("Parsing failed: " ++ str)

testParserHelperFail :: String -> IO ()
testParserHelperFail str = case runParser (parseExpresion) str of
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
        (Alias "alias abc def;\n")
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
