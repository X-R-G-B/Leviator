import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import AST

main :: IO ()
main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, computeTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Basic AST creation 0" $
      assertEqual "define x 42" (Node "define" [Leaf (Symbol "x"), Leaf (Number 42)]) (Node "define" [Leaf (Symbol "x"), Leaf (Number 42)])
  , testCase "Basic AST creation 1" $
      assertEqual "foo" (Leaf (Symbol "foo")) (Leaf (Symbol "foo"))
  , testCase "Basic AST creation 2" $
      assertEqual "42" (Leaf (Number 42)) (Leaf (Number 42))
  ]

computeTests :: TestTree
computeTests = testGroup "Compute tests"
  [ testCase "Basic compute 0" $
      assertEqual "define x 42" (Atom 42) (computeAst (Node "define" [Leaf (Symbol "x"), Leaf (Number 42)]))
  , testCase "Basic compute 1" $
      assertEqual "define x 42 and do x + x" (Leaf (Number 42)) (compute (Node "define" [Leaf (Symbol "x"), Leaf (Number 42)]))
  , testCase "Basic compute 2" $
      assertEqual "42 + 42" (Atom 84) (computeAst (Node "+" [Leaf (Number 42), Leaf (Number 42)]))
  ]
