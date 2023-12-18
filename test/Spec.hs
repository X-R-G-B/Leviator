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
    unitTestsASTParse,
    unitTestsComputeFunctions,
    unitTestsComputeConditions
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

computeAllAST :: Env -> [Tree] -> (Env, [Result])
computeAllAST env [] = (env, [])
computeAllAST env (x:xs) = do
  let (newEnv, result) = computeAST env x
  case result of
    Left (Just r) -> do
      let (newEnv2, results) = computeAllAST newEnv xs
      (newEnv2, (Left (Just r)):results)
    _ -> do
      let (newEnv2, results) = computeAllAST newEnv xs
      (newEnv2, results)

defaultEnv :: Env
defaultEnv = Env {defines = [], errors = [], functions = []}

unitTestComputeTypes :: TestTree
unitTestComputeTypes = testGroup "Tests Compute Types"
 [ testCase "bool true" $
       assertEqual "bool true"
         (defaultEnv, Left (Just (Boolean True)))
         (computeAST (defaultEnv) (Boolean True))
   , testCase "bool false" $
       assertEqual "bool false"
        (defaultEnv, Left (Just (Boolean False)))
         (computeAST (defaultEnv) (Boolean False))
    , testCase "number 42" $
        assertEqual "number 42"
          (defaultEnv, Left (Just (Number 42)))
          (computeAST (defaultEnv) (Number 42))
    , testCase "number -42" $
        assertEqual "number -42"
          (defaultEnv, Left (Just (Number (-42))))
          (computeAST (defaultEnv) (Number (-42)))
 ]

unitTestsComputeDefines :: TestTree
unitTestsComputeDefines = testGroup "Tests Compute defines"
  [ testCase "define x 42" $
      assertEqual "define x 42"
        (Env {defines = [Define {symbol = "x", expression = Number 42}], errors = [], functions = []}, Left (Nothing))
        (computeAST (defaultEnv) (List [Symbol "define", Symbol "x", Number 42]))
    , testCase "define x 42; x" $
      assertEqual "define x 42; x"
        (Env {defines = [Define {symbol = "x", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 42))])
        (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "x", Number 42]), (Symbol "x")])
    , testCase "define x 42; define y 84" $
      assertEqual "define x 42; define y 84"
        (Env {defines = [Define {symbol = "x", expression = Number 42}, Define {symbol = "y", expression = Number 84}], errors = [], functions = []}, [])
        (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "x", Number 42]), (List [Symbol "define", Symbol "y", Number 84])])
    , testCase "define x 42; define y 84; x; y" $
        assertEqual "define x 42; define y 84; x; y"
          (Env {defines = [Define {symbol = "x", expression = Number 42}, Define {symbol = "y", expression = Number 84}], errors = [], functions = []}, [Left (Just (Number 42)), Left (Just (Number 84))])
          (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "x", Number 42]), (List [Symbol "define", Symbol "y", Number 84]), (Symbol "x"), (Symbol "y")])
    , testCase "define x (42 + 6); x" $
        assertEqual "define x (42 + 6); x"
          (Env {defines = [Define {symbol = "x", expression = Number 48}], errors = [], functions = []}, [Left (Just (Number 48))])
          (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "x", (List [Symbol "+", Number 42, Number 6])]), (Symbol "x")])
    , testCase "define foo (4 + 5); foo + foo" $
        assertEqual "define foo (4 + 5); foo + foo"
          (Env {defines = [Define {symbol = "foo", expression = Number 9}], errors = [], functions = []}, [Left (Just (Number 18))])
          (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", (List [Symbol "+", Number 4, Number 5])]), (List [Symbol "+", Symbol "foo", Symbol "foo"])])
    , testCase "define foo 42; define bar foo; bar + bar" $
        assertEqual "define foo 42; define bar foo; bar + bar"
          (Env {defines = [Define {symbol = "foo", expression = Number 42}, Define {symbol = "bar", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 84))])
          (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "define", Symbol "bar", Symbol "foo"]), (List [Symbol "+", Symbol "bar", Symbol "bar"])])
    , testCase "define foo 42; define foo 84" $
        assertEqual "define foo 42; define foo 84"
          (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = ["Symbol foo is already defined"], functions = []}, [])
          (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "define", Symbol "foo", Number 84])])
  ]

unitTestsComputeSimpleFunctions :: TestTree
unitTestsComputeSimpleFunctions = testGroup "Tests compute + - div mod"
  [ testCase "42 + 42" $
      assertEqual "42 + 42"
        (defaultEnv, Left (Just (Number 84)))
        (computeAST (defaultEnv) (List [Symbol "+", Number 42, Number 42]))
    , testCase "-42 + -42" $
      assertEqual "-42 + -42"
        (defaultEnv, Left (Just (Number (-84))))
        (computeAST (defaultEnv) (List [Symbol "+", Number (-42), Number (-42)]))
    , testCase "42 + dontexist" $
      assertEqual "42 + dontexist"
        (Env {defines = [], errors = ["Symbol not found"], functions = []}, Right (undefined))
        (computeAST (defaultEnv) (List [Symbol "+", Number 42, Symbol "dontexist"]))
    , testCase "bool + number" $
      assertEqual "bool + number"
        (Env {defines = [], errors = ["Bad types in addition"], functions = []}, Right (undefined))
        (computeAST (defaultEnv) (List [Symbol "+", Boolean True, Number 42]))
    , testCase "20 / 2 + 3 * 5 - 10" $
      assertEqual "20 / 2 + 3 * 5 - 10"
        (defaultEnv, Left (Just (Number 15)))
        (computeAST (defaultEnv) (List [Symbol "-", (List [Symbol "+", (List [Symbol "div", Number 20, Number 2]), (List [Symbol "*", Number 3, Number 5])]), Number 10]))
    , testCase "11 mod 3" $
      assertEqual "11 mod 3"
        (defaultEnv, Left (Just (Number 2)))
        (computeAST (defaultEnv) (List [Symbol "mod", Number 11, Number 3]))
  ]

unitTestsComputeBasics :: TestTree
unitTestsComputeBasics = testGroup "Tests compute basics"
  [ testCase "define foo 42; foo + foo" $
      assertEqual "define foo 42; foo + foo"
        (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 84))])
        (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "foo"])])
    , testCase "define foo 42; define bar 42; foo + bar" $
        assertEqual "define foo 42; define bar 42; foo + bar"
          (Env {defines = [Define {symbol = "foo", expression = Number 42}, Define {symbol = "bar", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 84))])
          (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "define", Symbol "bar", Number 42]), (List [Symbol "+", Symbol "foo", Symbol "bar"])])
    , testCase "2 + 2 * 5" $
        assertEqual "2 + 2 * 5"
          (defaultEnv, [Left (Just (Number 12))])
          (computeAllAST (defaultEnv) [(List [Symbol "+", Number 2, (List [Symbol "*", Number 2, Number 5])])])
    , testCase "2 + 2 * (foo + 10) = 106" $
        assertEqual "2 + 2 * (foo + 10) = 106"
          (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 106))])
          (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "+", Number 2, (List [Symbol "*", Number 2, (List [Symbol "+", Symbol "foo", Number 10])])])])
    , testCase "2 + 3 * (8 + (5* ( 2 + 3))) = 107" $
        assertEqual "2 + 3 * (8 + (5* ( 2 + 3))) = 107"
          (defaultEnv, [Left (Just (Number 101))])
          (computeAllAST (defaultEnv) [(List [Symbol "+", Number 2, (List [Symbol "*", Number 3, (List [Symbol "+", Number 8, (List [Symbol "*", Number 5, (List [Symbol "+", Number 2, Number 3])])])])])])
    , testCase "div 42 0" $
        assertEqual "div 42 0"
          (Env {defines = [], errors = ["Division by 0"], functions = []}, Right (undefined))
          (computeAST (defaultEnv) (List [Symbol "div", Number 42, Number 0]))
  ]

unitTestsComputeFunctions :: TestTree
unitTestsComputeFunctions = testGroup "Tests compute functions"
  [ testCase "(define add (lambda (a b) (+ a b))); (add 1 2)" $
      assertEqual "(define add (lambda (a b) (+ a b))); (add 1 2)"
      (Env {defines = [], errors = [], functions = [Function {name = "add", params = ["a", "b"], bodies = [(List [Symbol "+", Symbol "a", Symbol "b"])]}]}, [Left (Just (Number 3))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "add", List [Symbol "lambda", List [Symbol "a", Symbol "b" ], List [Symbol "+", Symbol "a", Symbol "b"]]]), (List [Symbol "add", Number 1, Number 2])])
    , testCase "(define sub (lambda (a b) (- a b))); (sub 84 42)" $
      assertEqual "(define sub (lambda (a b) (- a b))); (sub 84 42)"
      (Env {defines = [], errors = [], functions = [Function {name = "sub", params = ["a", "b"], bodies = [(List [Symbol "-", Symbol "a", Symbol "b"])]}]}, [Left (Just (Number 42))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "sub", List [Symbol "lambda", List [Symbol "a", Symbol "b" ], List [Symbol "-", Symbol "a", Symbol "b"]]]), (List [Symbol "sub", Number 84, Number 42])])
    , testCase "(define sub (lambda (a b c d e) (+ a (+ b (* 8 (+ d e))); (sub 84 42 1 2 3)" $
      assertEqual "(define sub (lambda (a b c d e) (+ a (+ b (* 8 (+ d e))); (sub 84 42 1 2 3)"
      (Env {defines = [], errors = [], functions = [Function {name = "sub", params = ["a", "b", "c", "d", "e"], bodies = [(List [Symbol "+", Symbol "a", (List [Symbol "+", Symbol "b", (List [Symbol "*", Number 8, (List [Symbol "+", Symbol "d", Symbol "e"])])])])]}]}, [Left (Just (Number 166))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "sub", List [Symbol "lambda", List [Symbol "a", Symbol "b", Symbol "c", Symbol "d", Symbol "e" ], List [Symbol "+", Symbol "a", (List [Symbol "+", Symbol "b", (List [Symbol "*", Number 8, (List [Symbol "+", Symbol "d", Symbol "e"])])])]]]), (List [Symbol "sub", Number 84, Number 42, Number 1, Number 2, Number 3])])
    , testCase "(define func (lambda (a b) (define foo a) (+ foo b))); (func 1 2)" $
      assertEqual "(define func (lambda (a b) (define foo a) (+ foo b))); (func 1 2)"
      (Env {defines = [], errors = [], functions = [Function {name = "func", params = ["a", "b"], bodies = [(List [Symbol "define", Symbol "foo", Symbol "a", (List [Symbol "+", Symbol "foo", Symbol "b"])])]}]}, [Left (Just (Number 3))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "func", List [Symbol "lambda", List [Symbol "a", Symbol "b" ], List [Symbol "define", Symbol "foo", Symbol "a"], List [Symbol "+", Symbol "foo", Symbol "b"]]]), (List [Symbol "func", Number 1, Number 2])])
    , testCase "((lambda (a b) (+ a b)) 1 2)" $
      assertEqual "((lambda (a b) (+ a b)) 1 2)"
      (defaultEnv, Left (Just (Number 3)))
      (computeAST (defaultEnv) (List [List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]], List [Number 1, Number 2]]))
    , testCase "(define func (lambda () (define foo 42) (foo))); (func)" $
      assertEqual "(define func (lambda () (define foo 42) (foo))); (func)"
      (Env {defines = [], errors = [], functions = [Function {name = "func", params = [], bodies = [List [Symbol "define", Symbol "foo", Number 42], Symbol "foo"]}]}, [Left (Just (Number 42))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "func", List [Symbol "lambda", List [], List [Symbol "define", Symbol "foo", Number 42], Symbol "foo"]]), (List [Symbol "func"])])
    , testCase "(define func (lambda () (+ 42 42))); (func)" $
      assertEqual "(define func (lambda () (+ 42 42))); (func)"
      (Env {defines = [], errors = [], functions = [Function {name = "func", params = [], bodies = [List [Symbol "+", Number 42, Number 42]]}]}, [Left (Just (Number 84))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "func", List [Symbol "lambda", List [], List [Symbol "+", Number 42, Number 42]]]), (List [Symbol "func"])])
    , testCase "(define func (lambda (x) (+ 1 x))); (func 41)" $
      assertEqual "(define func (lambda (x) (+ 1 x))); (func 41)"
      (Env {defines = [], errors = [], functions = [Function {name = "func", params = ["x"], bodies = [List [Symbol "+", Number 1, Symbol "x"]]}]}, [Left (Just (Number 42))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "func", List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Number 1, Symbol "x"]]]), (List [Symbol "func", Number 41])])
    , testCase "(define (add a b) (+ a b)); (add 1 2)" $
      assertEqual "(define (add a b) (+ a b)); (add 1 2)"
      (Env {defines = [], errors = [], functions = [Function {name = "add", params = ["a", "b"], bodies = [List [Symbol "+", Symbol "a", Symbol "b"]]}]}, [Left (Just (Number 3))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", List [Symbol "add", Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]), (List [Symbol "add", Number 1, Number 2])])
    , testCase "(define (func x) (+ x 1)); (func 41)" $
      assertEqual "(define (func x) (+ x 1)); (func 41)"
      (Env {defines = [], errors = [], functions = [Function {name = "func", params = ["x"], bodies = [List [Symbol "+", Symbol "x", Number 1]]}]}, [Left (Just (Number 42))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", List [Symbol "func", Symbol "x"], List [Symbol "+", Symbol "x", Number 1]]), (List [Symbol "func", Number 41])])
    , testCase "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))); (fact 5)" $
      assertEqual "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))); (fact 5)"
      (Env {
        defines = [], errors = [], functions = [
          Function {
            name = "fact", params = ["n"], bodies = [
              (List [
                Symbol "if",
                List [
                  Symbol "<", Symbol "n", Number 2],
                Number 1,
                List [
                    Symbol "*", Symbol "n",
                    List [
                      Symbol "fact", List [
                        Symbol "-", Symbol "n", Number 1]
                    ]
                ]
              ]),
              [
                Left (Just (Number 120))
              ]
            ]
          }
        ]
      })
      (computeAllAST (defaultEnv) [(List [
                                            Symbol "if",
                                            List [
                                              Symbol "<", Symbol "n", Number 2],
                                            Number 1,
                                            List [
                                                Symbol "*", Symbol "n",
                                                List [
                                                  Symbol "fact", List [
                                                    Symbol "-", Symbol "n", Number 1]
                                                ]
                                            ]
                                          ]),
                                          (List [Symbol "func", Number 5])])
  ]

unitTestsComputeConditions :: TestTree
unitTestsComputeConditions = testGroup "Tests compute conditions"
  [ testCase "(if #t 42 84)" $
      assertEqual "(if #t 42 84)"
      (defaultEnv, Left (Just (Number 42)))
      (computeAST (defaultEnv) (List [Symbol "if", Boolean True, Number 42, Number 84]))
    , testCase "(if #f (3 + 3) (4 + 4))" $
      assertEqual "(if #f (3 + 3) (4 + 4))"
      (defaultEnv, Left (Just (Number 8)))
      (computeAST (defaultEnv) (List [Symbol "if", Boolean False, (List [Symbol "+", Number 3, Number 3]), (List [Symbol "+", Number 4, Number 4])]))
    , testCase "define foo 42; (if (< foo 10) (* foo 3) (div foo 2))" $
      assertEqual "define foo 42; (if (< foo 10) (* foo 3) (div foo 2))"
      (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 21))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "if", (List [Symbol "<", Symbol "foo", Number 10]), (List [Symbol "*", Symbol "foo", Number 3]), (List [Symbol "div", Symbol "foo", Number 2])])])
    , testCase "define foo 42; (if (eq? foo 42) (+ foo 42) (- foo 42))" $
      assertEqual "define foo 42; (if (eq? foo 42) (+ foo 42) (- foo 42))"
      (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 84))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "if", (List [Symbol "eq?", Symbol "foo", Number 42]), (List [Symbol "+", Symbol "foo", Number 42]), (List [Symbol "-", Symbol "foo", Number 42])])])
    , testCase "define foo 42; (if (eq? foo 22) (+ foo 42) (- foo 42))" $
      assertEqual "define foo 42; (if (eq? foo 22) (+ foo 42) (- foo 42))"
      (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = [], functions = []}, [Left (Just (Number 0))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "if", (List [Symbol "eq?", Symbol "foo", Number 22]), (List [Symbol "+", Symbol "foo", Number 42]), (List [Symbol "-", Symbol "foo", Number 42])])])
    , testCase "define foo 42; (if (diff? foo 22) (false) (true))" $
      assertEqual "define foo 42; (if (diff? foo 22) (false) (true))"
      (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = [], functions = []}, [Left (Just (Boolean True))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "if", (List [Symbol "diff?", Symbol "foo", Number 22]), Boolean True, Boolean False])])
    , testCase "define foo 42; (if (diff? foo 42) (true) (false))" $
      assertEqual "define foo 42; (if (diff? foo 42) (true) (false))"
      (Env {defines = [Define {symbol = "foo", expression = Number 42}], errors = [], functions = []}, [Left (Just (Boolean False))])
      (computeAllAST (defaultEnv) [(List [Symbol "define", Symbol "foo", Number 42]), (List [Symbol "if", (List [Symbol "diff?", Symbol "foo", Number 42]), Boolean True, Boolean False])])
    , testCase "(define foo 9); (define (func x) (if (< x 10) #t #f)); (func foo)" $
      assertEqual "(define foo 9); (define (func x) (if (< x 10) #t #f)); (func foo)"
      (Env {defines = [Define {symbol = "foo", expression = Number 9}], errors = [], functions = [
        Function {name = "func", params = ["x"], bodies = [
          (List [Symbol "if", (List [Symbol "<", Symbol "x", Number 10]), Boolean True, Boolean False])
          ]}
        ]}, [Left (Just (Boolean True))])
      (computeAllAST
        (defaultEnv)
        [
        (List [Symbol "define", Symbol "foo", Number 9]),
        (List [Symbol "define", (List [Symbol "func", Symbol "x"]), (List [Symbol "if", (List [Symbol "<", Symbol "x", Number 10]), Boolean True, Boolean False])]),
        (List [Symbol "func", Symbol "foo"])])
  ]
