import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import AST
--import TextToAST
import ComputeAST
import Defines

main :: IO ()
main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [unitTestsASTEqual, unitTestsComputeAst]--, unitTestsASTParse]

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

-- data Tree = Number Int64 | Symbol Symbol | Boolean Bool | List [Tree]

unitTestsComputeAst :: TestTree
unitTestsComputeAst = testGroup "AST Compute Tests"
  [ testCase "Basic AST compute 0" $
      assertEqual "define x 42; x"
        [Integer 42]
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", Number 42]), (Symbol "x")])
    , testCase "Basic AST compute 1" $
       assertEqual "42 + 42"
         [Number 84]
         (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "+", Number 42, Number 42])])
    , testCase "Basic AST compute 2" $
        assertEqual "2 + 2 * 5"
          [Number 12]
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "+", Number 2, (List [Symbol "*", Number 2, Number 5])])])
    , testCase "Basic AST compute 3" $
        assertEqual "2 + 2 * 5"
          [Number 12]
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "+", Number 2, (List [Symbol "*", Number 2, Number 5])])])
    , testCase "Basic AST compute 4" $
        assertEqual "define foo 42; foo + foo"
          [Number 84]
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "foo"])])
    , testCase "Basic AST compute 5" $
        assertEqual "define foo 42; define bar 42; foo + bar"
          [Number 84]
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "define", Symbol "bar", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "bar"])])
    , testCase "Basic AST compute 6" $
        assertEqual "true"
          [Bool True]
          (computeAllAST (Env {defines = [], errors = []}) [(Boolean True)])
    , testCase "Basic AST compute 7" $
        assertEqual "false"
          [Bool False]
          (computeAllAST (Env {defines = [], errors = []}) [(Boolean False)])
  ]
