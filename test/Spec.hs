import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import ComputeAST
import Types

main :: IO ()
main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests"
  [
    unitTestsASTEqual,
    unitTestComputeTypes,
    unitTestsComputeDefines,
    unitTestsComputeSimpleFunctions,
    unitTestsComputeBasics
  ]

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

computeAllAST :: Env -> [Tree] -> (Env, [Maybe Result])
computeAllAST env [] = (env, [])
computeAllAST env (x:xs) = do
  let (newEnv, result) = computeAST env x
  case result of
    Nothing -> computeAllAST newEnv xs
    Just tree -> do
      let (newEnv2, results) = computeAllAST newEnv xs
      (newEnv2, [Just tree] ++ results)

unitTestComputeTypes :: TestTree
unitTestComputeTypes = testGroup "Tests Compute Types"
 [ testCase "bool true" $
       assertEqual "bool true"
         (Env {defines = [], errors = []}, Just (Boolean True))
         (computeAST (Env {defines = [], errors = []}) (Boolean True))
   , testCase "bool false" $
       assertEqual "bool false"
        (Env {defines = [], errors = []}, Just (Boolean False))
         (computeAST (Env {defines = [], errors = []}) (Boolean False))
    , testCase "number 42" $
        assertEqual "number 42"
          (Env {defines = [], errors = []}, Just (Number 42))
          (computeAST (Env {defines = [], errors = []}) (Number 42))
    , testCase "number -42" $
        assertEqual "number -42"
          (Env {defines = [], errors = []}, Just (Number (-42)))
          (computeAST (Env {defines = [], errors = []}) (Number (-42)))
 ]

unitTestsComputeDefines :: TestTree
unitTestsComputeDefines = testGroup "Tests Compute defines"
  [ testCase "define x 42" $
      assertEqual "define x 42"
        (Env {defines = [Define {symbol = "x", expression = Number 42}], errors = []}, Nothing)
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "define", Symbol "x", Number 42]))
    , testCase "define x 42; x" $
      assertEqual "define x 42; x"
        (Env {defines = [Define {symbol = "x", expression = Number 42}], errors = []}, [Just (Number 42)])
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", Number 42]), (Symbol "x")])
    , testCase "define x 42; define y 84" $
      assertEqual "define x 42; define y 84"
        (Env {defines = [Define {symbol = "x", expression = Number 42}, Define {symbol = "y", expression = Number 84}], errors = []}, [])
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", Number 42]), (List [Symbol "define", Symbol "y", Number 84])])
    , testCase "define x 42; define y 84; x; y" $
        assertEqual "define x 42; define y 84; x; y"
          (Env {defines = [Define {symbol = "x", expression = Number 42}, Define {symbol = "y", expression = Number 84}], errors = []}, [Just (Number 42), Just (Number 84)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", Number 42]), (List [Symbol "define", Symbol "y", Number 84]), (Symbol "x"), (Symbol "y")])
    , testCase "define x (42 + 6); x" $
        assertEqual "define x (42 + 6); x"
          (Env {defines = [Define {symbol = "x", expression = List [Symbol "+", Number 42, Number 6]}], errors = []}, [Just (Number 48)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", (List [Symbol "+", Number 42, Number 6])]), (Symbol "x")])
  ]

unitTestsComputeSimpleFunctions :: TestTree
unitTestsComputeSimpleFunctions = testGroup "Tests compute + - div mod"
  [ testCase "42 + 42" $
      assertEqual "42 + 42"
        (Env {defines = [], errors = []}, Just (Number 84))
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "+", Number 42, Number 42]))
    , testCase "-42 + -42" $
      assertEqual "-42 + -42"
        (Env {defines = [], errors = []}, Just (Number (-84)))
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "+", Number (-42), Number (-42)]))
    , testCase "42 + dontexist" $
      assertEqual "42 + dontexist"
        (Env {defines = [], errors = ["Symbol not found"]}, Nothing)
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "+", Number 42, Symbol "dontexist"]))
    , testCase "bool + number" $
      assertEqual "bool + number"
        (Env {defines = [], errors = ["Bad types in addition"]}, Nothing)
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "+", Boolean True, Number 42]))
    , testCase "20 / 2 + 3 * 5 - 10" $
      assertEqual "20 / 2 + 3 * 5 - 10"
        (Env {defines = [], errors = []}, Just (Number 15))
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "-", (List [Symbol "+", (List [Symbol "div", Number 20, Number 2]), (List [Symbol "*", Number 3, Number 5])]), Number 10]))
    , testCase "11 mod 3" $
      assertEqual "11 mod 3"
        (Env {defines = [], errors = []}, Just (Number 2))
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "mod", Number 11, Number 3]))
  ]

unitTestsComputeBasics :: TestTree
unitTestsComputeBasics = testGroup "Tests compute basics"
  [ testCase "define foo 42; foo + foo" $
      assertEqual "define foo 42; foo + foo"
        (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = []}, [Just (Number 84)])
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "foo"])])
    , testCase "define foo 42; define bar 42; foo + bar" $
        assertEqual "define foo 42; define bar 42; foo + bar"
          (Env {defines = [Define {symbol = "foo", expression = Number 42}, Define {symbol = "bar", expression = Number 42}], errors = []}, [Just (Number 84)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "define", Symbol "bar", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "bar"])])
    , testCase "2 + 2 * 5" $
        assertEqual "2 + 2 * 5"
          (Env {defines = [], errors = []}, [Just (Number 12)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "+", Number 2, (List [Symbol "*", Number 2, Number 5])])])
    , testCase "2 + 2 * (foo + 10) = 106" $
        assertEqual "2 + 2 * (foo + 10) = 106"
          (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = []}, [Just (Number 106)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Number 2, (List [Symbol "*", Number 2, (List [Symbol "+", Symbol "foo", Number 10])])])])
    , testCase "2 + 3 * (8 + (5* ( 2 + 3))) = 107" $
        assertEqual "2 + 3 * (8 + (5* ( 2 + 3))) = 107"
          (Env {defines = [], errors = []}, [Just (Number 101)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "+", Number 2, (List [Symbol "*", Number 3, (List [Symbol "+", Number 8, (List [Symbol "*", Number 5, (List [Symbol "+", Number 2, Number 3])])])])])])
  ]
