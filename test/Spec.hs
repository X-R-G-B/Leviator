import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import AST
import TextToAST
import ComputeAST
import Defines

main :: IO ()
main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [unitTestsASTEqual, unitTestsASTParse, unitTestASTCompute]

unitTestsASTEqual :: TestTree
unitTestsASTEqual = testGroup "AST Equal Tests"
  [ testCase "Basic AST creation 0" $
      assertEqual "define x 42"
        (Node "define" (Just $ Leaf (Symbol "x")) (Just $ Leaf (Number 42)))
        (Node "define" (Just $ Leaf (Symbol "x")) (Just $ Leaf (Number 42)))
  , testCase "Basic AST creation 1" $
      assertEqual "foo"
        (Leaf (Symbol "foo"))
        (Leaf (Symbol "foo"))
  , testCase "Basic AST creation 2" $
      assertEqual "42"
        (Leaf (Number 42))
        (Leaf (Number 42))
  , testCase "Basic AST creation 3" $
      assertEqual "#f"
        (Leaf (Boolean False))
        (Leaf (Boolean False))
  , testCase "Basic AST creation 4" $
      assertEqual "#t"
        (Leaf (Boolean True))
        (Leaf (Boolean True))
  ]

unitTestsASTParse :: TestTree
unitTestsASTParse = testGroup "AST Parse Tests"
  [ testCase "(foo abc def hij)" $
      assertEqual "(foo abc def hij)"
        (Just $ Node "foo"
          (Just $ Leaf (Symbol "abc"))
          (Just $ Variadic
            (Just $ Leaf (Symbol "def"))
            (Just $ Leaf (Symbol "hij"))
          )
        )
        (textToAST "(foo abc def hij)")
  , testCase "(define x 42)" $
      assertEqual "(define x 42)"
        (Just $ Node "define"
          (Just $ Leaf (Symbol "x"))
          (Just $ Leaf (Number 42))
        )
        (textToAST "(define x 42)")
  , testCase "42" $
      assertEqual "42"
        (Just $ Leaf (Number 42))
        (textToAST "42")
  , testCase "#f" $
      assertEqual "#f"
        (Just $ Leaf (Boolean False))
        (textToAST "#f")
  , testCase "#t" $
      assertEqual "#t"
        (Just $ Leaf (Boolean True))
        (textToAST "#t")
  , testCase "foo" $
      assertEqual "foo"
        (Just $ Leaf (Symbol "foo"))
        (textToAST "foo")
  , testCase "(foo)" $
      assertEqual "(foo)"
        (Just $ Node "foo" Nothing Nothing)
        (textToAST "(foo)")
  , testCase "(foo def)" $
      assertEqual "(foo def)"
        (Just $ Node "foo"
          (Just $ Leaf (Symbol "def"))
          Nothing
        )
        (textToAST "(foo def)")
  , testCase "(foo def #t)" $
      assertEqual "(foo def #t)"
        (Just $ Node "foo"
          (Just $ Leaf (Symbol "def"))
          (Just $ Leaf (Boolean True))
        )
        (textToAST "(foo def #t)")
  , testCase "(foo def #f)" $
      assertEqual "(foo def #f)"
        (Just $ Node "foo"
          (Just $ Leaf (Symbol "def"))
          (Just $ Leaf (Boolean False))
        )
        (textToAST "(foo def #f)")
  , testCase "(foo #f def)" $
      assertEqual "(foo #f def)"
        (Just $ Node "foo"
          (Just $ Leaf (Boolean False))
          (Just $ Leaf (Symbol "def"))
        )
        (textToAST "(foo #f def)")
  , testCase "(foo def #t #f)" $
      assertEqual "(foo def #t #f)"
        (Just $ Node "foo"
          (Just $ Leaf (Symbol "def"))
          (Just $ Variadic
            (Just $ Leaf (Boolean True))
            (Just $ Leaf (Boolean False))
          )
        )
        (textToAST "(foo def #t #f)")
  , testCase "(foo def #f #t)" $
      assertEqual "(foo def #f #t)"
        (Just $ Node "foo"
          (Just $ Leaf (Symbol "def"))
          (Just $ Variadic
            (Just $ Leaf (Boolean False))
            (Just $ Leaf (Boolean True))
          )
        )
        (textToAST "(foo def #f #t)")
  , testCase "(fst 1 (scd 2 3 4))" $
      assertEqual "(fst 1 (scd 2 3 4))"
        (Just $ Node "fst"
          (Just $ Leaf (Number 1))
          (Just $ Node "scd"
            (Just $ Leaf (Number 2))
            (Just $ Variadic
              (Just $ Leaf (Number 3))
              (Just $ Leaf (Number 4))
            )
          )
        )
        (textToAST "(fst 1 (scd 2 3 4))")
  , testCase "(foo 42 )" $
      assertEqual "(foo 42 )"
        (Just $ Node "foo"
          (Just $ Leaf (Number 42))
          Nothing
        )
        (textToAST "(foo 42 )")
  , testCase "(foo def )" $
      assertEqual "(foo 42 )"
        (Just $ Node "foo"
          (Just $ Leaf (Symbol "def"))
          Nothing
        )
        (textToAST "(foo def )")
  ]

unitTestASTCompute :: TestTree
unitTestASTCompute = testGroup "AST compute Tests"
    [ testCase "test1" $
        assertEqual "number 21 + number 21 = 42"
          [Number 42]
          (computeAllAST (Env []) [Node "+" (Just (Leaf (Number 21))) (Just (Leaf (Number 21)))])
    , testCase "test2" $
        assertEqual "define foo 42 and tree with leaf foo"
          [Number 42]
          (computeAllAST (Env []) [(Node "define" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 42)))), (Leaf (Symbol "foo"))])
    ]
