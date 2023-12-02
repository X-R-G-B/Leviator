import Test.Tasty
import Test.Tasty.HUnit

import AST

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Basic AST creation 0" $
      assertEqual "define x 42" (Node "define" [Leaf (Symbol "x"), Leaf (Number 42)]) (Node "define" [Leaf (Symbol "x"), Leaf (Number 42)])
  , testCase "Basic AST creation 1" $
      assertEqual "foo" (Leaf (Symbol "foo")) (Leaf (Symbol "foo"))
  , testCase "Basic AST creation 2" $
      assertEqual "42" (Leaf (Number 42)) (Leaf (Number 42))
  ]
