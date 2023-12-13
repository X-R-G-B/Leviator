import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import Computing.ComputeAST
import Types
import Parsing.Parser

main :: IO ()
main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests"
  [
    unitTestsASTEqual,
    unitTestComputeTypes,
    unitTestsComputeDefines,
    unitTestsComputeSimpleFunctions,
    unitTestsComputeBasics,
    unitTestsASTParse
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

testParser :: String -> Tree -> IO ()
testParser str tree = case runParser (parseTree) str of
  Nothing -> assertFailure "Parsing failed"
  Just (t, _) -> assertEqual str tree t

unitTestsASTParse :: TestTree
unitTestsASTParse = testGroup "AST Parse Tests"
  [ testCase "(foo abc def hij)" $
       testParser "(foo abc def hij)" (List [Symbol "foo", Symbol "abc", Symbol "def", Symbol "hij"])
  , testCase "(define x 42)" $
       testParser "(define x 42)" (List [Symbol "define", Symbol "x", Number 42])
  , testCase "42" $
       testParser "42" (Number 42)
  , testCase "#f" $
       testParser "#f" (Boolean False)
  , testCase "#t" $
       testParser "#t" (Boolean True)
  , testCase "foo" $
       testParser "foo" (Symbol "foo")
  , testCase "(foo)" $
       testParser "(foo)" (List [Symbol "foo"])
  , testCase "(foo def)" $
       testParser "(foo def)" (List [Symbol "foo", Symbol "def"])
  , testCase "(foo def #t)" $
       testParser "(foo def #t)" (List [Symbol "foo", Symbol "def", Boolean True])
  , testCase "(foo def #f)" $
       testParser "(foo def #f)" (List [Symbol "foo", Symbol "def", Boolean False])
  , testCase "(foo #f def)" $
       testParser "(foo #f def)" (List [Symbol "foo", Boolean False, Symbol "def"])
  , testCase "(foo def #t #f)" $
       testParser "(foo def #t #f)" (List [Symbol "foo", Symbol "def", Boolean True, Boolean False])
  , testCase "(foo def #f #t)" $
       testParser "(foo def #f #t)" (List [Symbol "foo", Symbol "def", Boolean False, Boolean True])
  , testCase "(fst 1 (scd 2 3 4))" $
       testParser "(fst 1 (scd 2 3 4))" (List [Symbol "fst", Number 1, List [Symbol "scd", Number 2, Number 3, Number 4]])
  , testCase "(fst 1 (scd 2 3 4) 12)" $
       testParser "(fst 1 (scd 2 3 4) 12)" (List [Symbol "fst", Number 1, List [Symbol "scd", Number 2, Number 3, Number 4], Number 12])
  , testCase "(foo 42 )" $
       testParser "(foo 42 )" (List [Symbol "foo", Number 42])
  , testCase "(foo def )" $
       testParser "(foo def )" (List [Symbol "foo", Symbol "def"])
  , testCase "(foo ((def)) #t)" $
       testParser "(foo ((def)) #t)" (List [Symbol "foo", List [List [Symbol "def"]], Boolean True])
  , testCase "(do (re (mi)) 12)" $
       testParser "(do (re (mi)) 12)" (List [Symbol "do", List [Symbol "re", List [Symbol "mi"]], Number 12])
  , testCase "(do (re (mi)) 12 (re (mi)))" $
       testParser "(do (re (mi)) 12 (re (mi)))" (List [Symbol "do", List [Symbol "re", List [Symbol "mi"]], Number 12, List [Symbol "re", List [Symbol "mi"]]])
  ]

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
