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
    [testReturn, testFuncCall, testVars, testWhile]

testReturn :: TestTree
testReturn = testGroup "testReturn"
  [
    testCase "test no start" (shouldBeFalse
      [((True, "main", [], "Int"), [Return (Integer 0)])]),
    testCase "testReturn" (shouldBeTrue
      [((True, "start", [], "Int"), ([Return (Integer 0)]))]),
    testCase "testWrongReturnType" (shouldBeFalse
      [((True, "start", [], "Int"), ([Return (Boolean True)]))]),
    testCase "test return param" (shouldBeFalse
      [((True, "start", [("a", "Int"), ("b", "Bool")], "Int"), [Return (Var "b")])])
  ]

testFuncCall :: TestTree
testFuncCall = testGroup "testFuncCall"
  [
    testCase "test recursive" (shouldBeTrue
      [((True, "start", [], "Int"), [Return (FuncValue ("start", []))])]),
    testCase "test return wrong type" (shouldBeFalse
      [((True, "start", [], "Int"), [Return (Integer 0)]), ((True, "snd", [], "Bool"), [Return (FuncValue ("start", []))])]),
    testCase "test call with params" (shouldBeTrue
      [((True, "start", [("a", "Int"), ("b", "Bool")], "Int"), [Return (Var "a")]), ((True, "snd", [], "Int"), [Return (FuncValue ("start", [(Integer 0), (Boolean True)]))])]),
    testCase "test call with wrongs params" (shouldBeFalse
      [((True, "start", [("a", "Int"), ("b", "Bool")], "Int"), [Return (Var "a")]), ((True, "snd", [], "Int"), [Return (FuncValue ("start", [(Integer 0), (Integer 2)]))])])
  ]

testVars :: TestTree
testVars = testGroup "testVars"
  [
    testCase "test vars" (shouldBeTrue
      [((True, "start", [], "Int"), [Declaration (("a", "Int"), Integer 0), Assignation ("a", (FuncValue ("+", [(Var "a"), (Integer 1)]))), Return (Var "a")])]),
    testCase "test params" (shouldBeTrue
      [((True, "start", [("a", "Int")], "Int"), [Assignation ("a", (FuncValue ("+", [(Var "a"), (Integer 1)]))), Return (Var "a")])]),
    testCase "test params" (shouldBeTrue
      [((True, "start", [("a", "Int")], "Int"), [Declaration (("b", "Int"), (Var "a")), Declaration (("c", "Int"), (Var "b")), Return (Var "c")])]),
    testCase "test wrong var declaration" (shouldBeFalse
      [((True, "start", [("a", "Int")], "Int"), [Declaration (("a", "Int"), Integer 0)])]),
    testCase "test double func name" (shouldBeFalse
      [((True, "start", [("a", "Int")], "Int"), [Return (Integer 0)]), ((True, "start", [("a", "Int")], "Int"), [Return (Integer 0)])])
  ]

testWhile :: TestTree
testWhile = testGroup "testWhile"
  [
    testCase "test while" (shouldBeTrue
      [((True, "start", [], "Int"), [While (Boolean True, [Return (Integer 0)])])]),
    testCase "test while" (shouldBeFalse
      [((True, "start", [], "Int"), [While (Integer 0, [Return (Integer 0)])])]),
    testCase "test while" (shouldBeFalse
      [((True, "start", [], "Int"), [While (Boolean True, [Return (Boolean True)])])])
  ]

shouldBeTrue :: [FuncDeclaration] -> Assertion
shouldBeTrue funcs = (typeCheck funcs) @?= True

shouldBeFalse :: [FuncDeclaration] -> Assertion
shouldBeFalse funcs = (typeCheck funcs) @?= False
