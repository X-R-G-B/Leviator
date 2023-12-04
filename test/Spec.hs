import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import AST
import TextToAST

main :: IO ()
main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [unitTestsASTEqual, unitTestsASTParse]

unitTestsASTEqual :: TestTree
unitTestsASTEqual = testGroup "AST Equal Tests"
  [ testCase "Basic AST creation 0" $
      assertEqual "define x 42" (Node "define" (Just $ Leaf (Symbol "x")) (Just $ Leaf (Number 42))) (Node "define" (Just $ Leaf (Symbol "x")) (Just $ Leaf (Number 42)))
  , testCase "Basic AST creation 1" $
      assertEqual "foo" (Leaf (Symbol "foo")) (Leaf (Symbol "foo"))
  , testCase "Basic AST creation 2" $
      assertEqual "42" (Leaf (Number 42)) (Leaf (Number 42))
  , testCase "Basic AST creation 3" $
      assertEqual "#f" (Leaf (Boolean False)) (Leaf (Boolean False))
  , testCase "Basic AST creation 4" $
      assertEqual "#t" (Leaf (Boolean True)) (Leaf (Boolean True))
  ]

unitTestsASTParse :: TestTree
unitTestsASTParse = testGroup "AST Parse Tests"
  [ testCase "Basic AST creation 0" $
      assertEqual (textToAST "(foo abc def hij)") (Just $ (Node "foo" (Leaf (Symbol "abc"))) (Varidadic (Leaf (Symbol "def") (Leaf (Symbol "hij")))))
  ]
