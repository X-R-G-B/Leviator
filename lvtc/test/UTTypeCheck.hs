{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- UTShuntingYard
-}

module UTTypeCheck (
  uTTypeCheck
) where

import Test.Tasty
import Test.Tasty.HUnit
import AST
import TypeCheck

uTTypeCheck :: TestTree
uTTypeCheck = testGroup "typeCheck tests"
    [testReturn, testFuncCall, testVars]

testReturn :: TestTree
testReturn = testGroup "testReturn"
  [
    testCase "testReturn" (shouldBeTrue
      [((True, "main", [], "Int"), ([Return (Integer 0)]))]),
    testCase "testWrongReturnType" (shouldBeFalse
      [((True, "main", [], "Int"), ([Return (Boolean True)]))]),
    testCase "test return param" (shouldBeFalse
      [((True, "main", [("a", "Int"), ("b", "Bool")], "Int"), [Return (Var "b")])])
  ]

testFuncCall :: TestTree
testFuncCall = testGroup "testFuncCall"
  [
    testCase "test recursive" (shouldBeTrue
      [((True, "main", [], "Int"), [Return (FuncValue ("main", []))])]),
    testCase "test return wrong type" (shouldBeFalse
      [((True, "main", [], "Int"), [Return (Integer 0)]), ((True, "snd", [], "Bool"), [Return (FuncValue ("main", []))])]),
    testCase "test call with params" (shouldBeTrue
      [((True, "main", [("a", "Int"), ("b", "Bool")], "Int"), [Return (Var "a")]), ((True, "snd", [], "Int"), [Return (FuncValue ("main", [(Integer 0), (Boolean True)]))])]),
    testCase "test call with wrongs params" (shouldBeFalse
      [((True, "main", [("a", "Int"), ("b", "Bool")], "Int"), [Return (Var "a")]), ((True, "snd", [], "Int"), [Return (FuncValue ("main", [(Integer 0), (Integer 2)]))])])
  ]

testVars :: TestTree
testVars = testGroup "testVars"
  [
    testCase "test vars" (shouldBeTrue
      [((True, "main", [], "Int"), [Declaration (("a", "Int"), Integer 0), Assignation ("a", (FuncValue ("+", [(Var "a"), (Integer 1)]))), Return (Var "a")])]),
    testCase "test params" (shouldBeTrue
      [((True, "main", [("a", "Int")], "Int"), [Assignation ("a", (FuncValue ("+", [(Var "a"), (Integer 1)]))), Return (Var "a")])]),
    testCase "test params" (shouldBeTrue
      [((True, "main", [("a", "Int")], "Int"), [Declaration (("b", "Int"), (Var "a")), Declaration (("c", "Int"), (Var "b")), Return (Var "c")])]),
    testCase "test wrong var declaration" (shouldBeFalse
      [((True, "main", [("a", "Int")], "Int"), [Declaration (("a", "Int"), Integer 0)])]),
    testCase "test double func name" (shouldBeFalse
      [((True, "main", [("a", "Int")], "Int"), [Return (Integer 0)]), ((True, "main", [("a", "Int")], "Int"), [Return (Integer 0)])])
  ]

shouldBeTrue :: [FuncDeclaration] -> Assertion
shouldBeTrue funcs = (typeCheck funcs) @?= True

shouldBeFalse :: [FuncDeclaration] -> Assertion
shouldBeFalse funcs = (typeCheck funcs) @?= False
