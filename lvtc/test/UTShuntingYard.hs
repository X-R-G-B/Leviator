module UTShuntingYard (
    utShuntingYard
) where

import Test.Tasty
import Test.Tasty.HUnit

import ShuntingYard
import AST

utShuntingYard :: TestTree
utShuntingYard = testGroup "ShuntingYard"
  [
    testCase "basic" $
      assertEqual "3+5"
        (SYS [] [Integer 3, Integer 5, Var "+"])
        basic_end
  , testCase "basic basic" $
      assertEqual "3+4+5"
        (SYS [] [Integer 3, Integer 4, Var "+", Integer 5, Var "+"])
        basic_basic_end
  , testCase "priority *" $
      assertEqual "3+4*5"
        (SYS [] [Integer 3, Integer 4, Integer 5, Var "*", Var "+"])
        priority_mul_end
  , testCase "priority /" $
      assertEqual "3-4/5"
        (SYS [] [Integer 3, Integer 4, Integer 5, Var "/", Var "-"])
        priority_div_end
  , testCase "3+4*2/(1-5)" $
      assertEqual "3+4*2/(1-5)"
        (SYS [] [Integer 3, Integer 4, Integer 2, Var "*", Integer 1, Integer 5, Var "-", Var "/", Var "+"])
        priority_end
  ]
  where
    basic_1 = shuntingYardValue (Integer 3) (SYS [] [])
    basic_2 = shuntingYardOp (Var "+") basic_1
    basic_3 = shuntingYardValue (Integer 5) basic_2
    basic_end = shuntingYardEnd basic_3
    --
    basic_basic_1 = shuntingYardValue (Integer 3) (SYS [] [])
    basic_basic_2 = shuntingYardOp (Var "+") basic_basic_1
    basic_basic_3 = shuntingYardValue (Integer 4) basic_basic_2
    basic_basic_4 = shuntingYardOp (Var "+") basic_basic_3
    basic_basic_5 = shuntingYardValue (Integer 5) basic_basic_4
    basic_basic_end = shuntingYardEnd basic_basic_5
    --
    priority_mul_1 = shuntingYardValue (Integer 3) (SYS [] [])
    priority_mul_2 = shuntingYardOp (Var "+") priority_mul_1
    priority_mul_3 = shuntingYardValue (Integer 4) priority_mul_2
    priority_mul_4 = shuntingYardOp (Var "*") priority_mul_3
    priority_mul_5 = shuntingYardValue (Integer 5) priority_mul_4
    priority_mul_end = shuntingYardEnd priority_mul_5
    --
    priority_div_1 = shuntingYardValue (Integer 3) (SYS [] [])
    priority_div_2 = shuntingYardOp (Var "-") priority_div_1
    priority_div_3 = shuntingYardValue (Integer 4) priority_div_2
    priority_div_4 = shuntingYardOp (Var "/") priority_div_3
    priority_div_5 = shuntingYardValue (Integer 5) priority_div_4
    priority_div_end = shuntingYardEnd priority_div_5
    --
    priority_1 = shuntingYardValue (Integer 3) (SYS [] [])
    priority_2 = shuntingYardOp (Var "+") priority_1
    priority_3 = shuntingYardValue (Integer 4) priority_2
    priority_4 = shuntingYardOp (Var "*") priority_3
    priority_5 = shuntingYardValue (Integer 2) priority_4
    priority_6 = shuntingYardOp (Var "/") priority_5
    priority_7 = shuntingYardOp (Var "(") priority_6
    priority_8 = shuntingYardValue (Integer 1) priority_7
    priority_9 = shuntingYardOp (Var "-") priority_8
    priority_10 = shuntingYardValue (Integer 5) priority_9
    priority_11 = shuntingYardOp (Var ")") priority_10
    priority_end = shuntingYardEnd priority_11
