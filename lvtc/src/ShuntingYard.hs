{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ShuntingYard
-}

module ShuntingYard
(
    shuntingYardOp,
    shuntingYardEnd,
    shuntingYardValue,
    ShuntingYardState (..)
) where

import AST

data ShuntingYardState = SYS [Value] [Value]

instance Eq ShuntingYardState where
    (==) (SYS y z) (SYS y' z') = y == y' && z == z'

instance Show ShuntingYardState where
    show (SYS y z) = "SYS[< " ++ show y ++ " >< " ++ show z ++ " >]"

shuntingYardValue :: Value -> ShuntingYardState -> ShuntingYardState
shuntingYardValue val (SYS ops out) = SYS ops (out ++ [val])

opOnStack :: Value -> ShuntingYardState -> ShuntingYardState
opOnStack (Var "+") (SYS ((Var "*"):ops) out) =
    opOnStack (Var "+") (SYS ops (out ++ [Var "*"]))
opOnStack (Var "+") (SYS ((Var "/"):ops) out) =
    opOnStack (Var "+") (SYS ops (out ++ [Var "/"]))
opOnStack (Var "+") (SYS ((Var "-"):ops) out) =
    opOnStack (Var "+") (SYS ops (out ++ [Var "-"]))
opOnStack (Var "+") (SYS ((Var "+"):ops) out) =
    opOnStack (Var "+") (SYS ops (out ++ [Var "+"]))
opOnStack (Var "-") (SYS ((Var "*"):ops) out) =
    opOnStack (Var "-") (SYS ops (out ++ [Var "*"]))
opOnStack (Var "-") (SYS ((Var "/"):ops) out) =
    opOnStack (Var "-") (SYS ops (out ++ [Var "/"]))
opOnStack (Var "-") (SYS ((Var "+"):ops) out) =
    opOnStack (Var "-") (SYS ops (out ++ [Var "+"]))
opOnStack (Var "-") (SYS ((Var "-"):ops) out) =
    opOnStack (Var "-") (SYS ops (out ++ [Var "-"]))
opOnStack (Var "*") (SYS ((Var "/"):ops) out) =
    opOnStack (Var "*") (SYS ops (out ++ [Var "/"]))
opOnStack (Var "*") (SYS ((Var "*"):ops) out) =
    opOnStack (Var "*") (SYS ops (out ++ [Var "*"]))
opOnStack (Var "/") (SYS ((Var "*"):ops) out) =
    opOnStack (Var "/") (SYS ops (out ++ [Var "*"]))
opOnStack (Var "/") (SYS ((Var "/"):ops) out) =
    opOnStack (Var "/") (SYS ops (out ++ [Var "/"]))
opOnStack _ sys = sys

shuntingYardOp :: Value -> ShuntingYardState -> ShuntingYardState
shuntingYardOp (Var "(") (SYS ops out) =
    SYS ((Var "(") : ops) out
shuntingYardOp (Var ")") (SYS [] _) =
    SYS [] []
shuntingYardOp (Var ")") (SYS ((Var "("):ops) out) =
    SYS ops out
shuntingYardOp (Var ")") (SYS (op:ops) out) =
    shuntingYardOp (Var ")") (SYS ops (out ++ [op]))
shuntingYardOp (Var op) sys =
    SYS ((Var op):ops') out'
    where
        (SYS ops' out') = opOnStack (Var op) sys
shuntingYardOp _ _ = SYS [] []

shuntingYardEnd :: ShuntingYardState -> ShuntingYardState
shuntingYardEnd (SYS [] out) = SYS [] out
shuntingYardEnd (SYS (op:ops) out) = shuntingYardEnd (SYS ops (out ++ [op]))
