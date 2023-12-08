import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import AST
--import TextToAST
--import ComputeAST
--import Defines

main :: IO ()
main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [unitTestsASTEqual]--, unitTestsASTParse, unitTestASTCompute]

unitTestsASTEqual :: TestTree
unitTestsASTEqual = testGroup "AST Equal Tests"
  [ testCase "Basic AST creation 0" $
      assertEqual "define x 42"
        (List [Symbol "define", Symbol "x", Number 42])
        (List [Symbol "define", Symbol "x", Number 42])
  , testCase "Basic AST creation 1" $
      assertEqual "foo"
        (Symbol "foo")
        (Symbol "foo")
  , testCase "Basic AST creation 2" $
      assertEqual "42"
        (Number 42)
        (Number 42)
  , testCase "Basic AST creation 3" $
      assertEqual "#f"
        (Boolean False)
        (Boolean False)
  , testCase "Basic AST creation 4" $
      assertEqual "#t"
        (Boolean True)
        (Boolean True)
  ]

--unitTestsASTParse :: TestTree
--unitTestsASTParse = testGroup "AST Parse Tests"
--  [ testCase "(foo abc def hij)" $
--      assertEqual "(foo abc def hij)"
--        (textToAST "(foo abc def hij)")
--  , testCase "(define x 42)" $
--      assertEqual "(define x 42)"
--        (textToAST "(define x 42)")
--  , testCase "42" $
--      assertEqual "42"
--        (textToAST "42")
--  , testCase "#f" $
--      assertEqual "#f"
--        (textToAST "#f")
--  , testCase "#t" $
--      assertEqual "#t"
--        (textToAST "#t")
--  , testCase "foo" $
--      assertEqual "foo"
--        (textToAST "foo")
--  , testCase "(foo)" $
--      assertEqual "(foo)"
--        (textToAST "(foo)")
--  , testCase "(foo def)" $
--      assertEqual "(foo def)"
--        (textToAST "(foo def)")
--  , testCase "(foo def #t)" $
--      assertEqual "(foo def #t)"
--        (textToAST "(foo def #t)")
--  , testCase "(foo def #f)" $
--      assertEqual "(foo def #f)"
--        (textToAST "(foo def #f)")
--  , testCase "(foo #f def)" $
--      assertEqual "(foo #f def)"
--        (textToAST "(foo #f def)")
--  , testCase "(foo def #t #f)" $
--      assertEqual "(foo def #t #f)"
--        (textToAST "(foo def #t #f)")
--  , testCase "(foo def #f #t)" $
--      assertEqual "(foo def #f #t)"
--        (textToAST "(foo def #f #t)")
--  , testCase "(fst 1 (scd 2 3 4))" $
--      assertEqual "(fst 1 (scd 2 3 4))"
--        (textToAST "(fst 1 (scd 2 3 4))")
--  , testCase "(fst 1 (scd 2 3 4) 12)" $
--      assertEqual "(fst 1 (scd 2 3 4) 12)"
--        (textToAST "(fst 1 (scd 2 3 4) 12)")
--  , testCase "(foo 42 )" $
--      assertEqual "(foo 42 )"
--        (textToAST "(foo 42 )")
--  , testCase "(foo def )" $
--      assertEqual "(foo def )"
--        (textToAST "(foo def )")
--  , testCase "(foo ((def)) #t)" $
--      assertEqual "(foo ((def)) #t)"
--        (textToAST "(foo ((def)) #t)")
--  , testCase "(do (re (mi)) 12)" $
--      assertEqual "(do (re (mi)) 12)"
--        (textToAST "(do (re (mi)) 12)")
--  , testCase "(do (re (mi)) 12 (re (mi)))" $
--      assertEqual "(do (re (mi)) 12 (re (mi)))"
--        (textToAST "(do (re (mi)) 12 (re (mi)))")
--  ]
--
--unitTestASTCompute :: TestTree
--unitTestASTCompute = testGroup "AST compute Tests"
--    [ testCase "test1" $
--        assertEqual "number 21 + number 21 = 42"
--          [Number 42]
--          computeAllAST (Env []) [(List [Symbol "+", Number 21, Number 21])]
--    , testCase "test2" $
--        assertEqual "define foo 42 and tree with Symbol foo"
--          [Number 42]
--          computeAllAST (Env []) [(List [Symbol "define", Symbol "foo", Number 42]), (Symbol "foo")]
--    , testCase "test3" $
--        assertEqual "define foo 42 and do foo + 42"
--          [Number 84]
--          computeAllAST (Env []) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Symbol "foo", Number 42])]
--    , testCase "test3" $
--        assertEqual "define foo 42 and do 42 + foo"
--          [Number 84]
--          computeAllAST (Env []) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Number 42, Symbol "foo")])]
--    , testCase "test5" $
--        assertEqual "define foo 42 and do foo + foo"
--          [Number 84]
--          computeAllAST (Env []) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "foo"])]
--    , testCase "test6" $
--        assertEqual "define foo 42 and bar 21 and do foo + bar"
--          [Number 63]
--          computeAllAST (Env []) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "define", Symbol "bar", Number 21]), (List [Symbol "+", Symbol "foo", Symbol "bar"])]
--    , testCase "test7" $
--        assertEqual "2 + (5 * 2) (result = 12)"
--          [Number 12]
--          computeAllAST (Env []) [(List [Symbol "+", Number 2, List [Symbol "*", Number 5, Number 2])])]
--    , testCase "test8" $
--        assertEqual "define foo 42 and (2 * 5) + (foo / 2) (result = 10 + 21 = 31)"
--          [Number 31]
--          computeAllAST (Env []) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", List [Symbol "*", Number 2, Number 5]), List [Symbol "/", Symbol "foo", Number 2])])]
--    , testCase "test9" $
--        assertEqual "2 + 2 + (5 * 2) (result = 14)"
--          [Number 14]
--          computeAllAST (Env []) [(List [Symbol "+", List [Symbol "+", Number 2, Number 2]), List [Symbol "*", Number 5, Number 2])])]
--    , testCase "test10" $
--        assertEqual "14 mod 5 (result = 4)"
--          [Number 4]
--          computeAllAST (Env []) [(List [Symbol "mod", Number 14, Number 5])]
--    ]
--
