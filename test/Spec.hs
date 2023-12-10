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
 [ testCase "Basic compute AST Types 0" $
       assertEqual "bool true"
         (Env {defines = [], errors = []}, Just (Boolean True))
         (computeAST (Env {defines = [], errors = []}) (Boolean True))
   , testCase "Basic compute AST Types 1" $
       assertEqual "bool false"
        (Env {defines = [], errors = []}, Just (Boolean False))
         (computeAST (Env {defines = [], errors = []}) (Boolean False))
    , testCase "Basic compute AST Types 2" $
        assertEqual "number 42"
          (Env {defines = [], errors = []}, Just (Number 42))
          (computeAST (Env {defines = [], errors = []}) (Number 42))
    , testCase "Basic compute AST Types 3" $
        assertEqual "number -42"
          (Env {defines = [], errors = []}, Just (Number (-42)))
          (computeAST (Env {defines = [], errors = []}) (Number (-42)))
 ]

unitTestsComputeDefines :: TestTree
unitTestsComputeDefines = testGroup "Tests Compute defines"
  [ testCase "DEFINES AST compute 0" $
      assertEqual "define x 42"
        (Env {defines = [Define {symbol = "x", expression = Number 42}], errors = []}, Nothing)
        (computeAST (Env {defines = [], errors = []}) (List [Symbol "define", Symbol "x", Number 42]))
    , testCase "DEFINES AST compute 1" $
      assertEqual "define x 42; x"
        (Env {defines = [Define {symbol = "x", expression = Number 42}], errors = []}, [Just (Number 42)])
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", Number 42]), (Symbol "x")])
    , testCase "DEFINES AST compute 2" $
      assertEqual "define x 42; define y 84"
        (Env {defines = [Define {symbol = "x", expression = Number 42}, Define {symbol = "y", expression = Number 84}], errors = []}, [])
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", Number 42]), (List [Symbol "define", Symbol "y", Number 84])])
    , testCase "DEFINES AST compute 3" $
        assertEqual "define x 42; define y 84; x; y"
          (Env {defines = [Define {symbol = "x", expression = Number 42}, Define {symbol = "y", expression = Number 84}], errors = []}, [Just (Number 42), Just (Number 84)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "x", Number 42]), (List [Symbol "define", Symbol "y", Number 84]), (Symbol "x"), (Symbol "y")])
  ]

unitTestsComputeSimpleFunctions :: TestTree
unitTestsComputeSimpleFunctions = testGroup "Tests compuite + - div mod"
  [ testCase "+ - div mod AST compute 0" $
      assertEqual "42 + 42"
        (Env {defines = [], errors = []}, [Just (Number 84)])
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "+", Number 42, Number 42])])
  ]

unitTestsComputeBasics :: TestTree
unitTestsComputeBasics = testGroup "Tests compute basics"
  [ testCase "BASICS AST compute 0" $
      assertEqual "define foo 42; foo + foo"
        (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = []}, [Just (Number 84)])
        (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "foo"])])
    , testCase "BASICS AST compute 1" $
        assertEqual "define foo 42; define bar 42; foo + bar"
          (Env {defines = [Define {symbol = "foo", expression = Number 42}, Define {symbol = "bar", expression = Number 42}], errors = []}, [Just (Number 84)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "define", Symbol "bar", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "bar"])])
    , testCase "BASICS AST compute 2" $
        assertEqual "2 + 2 * 5"
          (Env {defines = [], errors = []}, [Just (Number 12)])
          (computeAllAST (Env {defines = [], errors = []}) [(List [Symbol "+", Number 2, (List [Symbol "*", Number 2, Number 5])])])
  ]
