{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import ComputeAST
import AST
import Defines

-- Foo = 42
testTreeDefine :: Tree
testTreeDefine = Node "define" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 42)))

-- Bar = 21
testTreeDefine2 :: Tree
testTreeDefine2 = Node "define" (Just (Leaf (Symbol "bar"))) (Just (Leaf (Number 21)))

-- 21 + 21 (result = 42)
testTreeAddition :: Tree
testTreeAddition = Node "+" (Just (Leaf (Number 21))) (Just (Leaf (Number 21)))

-- Foo + 42 (result = 42 + 42 = 84)
testTreeAdditiondefine :: Tree
testTreeAdditiondefine = Node "+" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 42)))

-- Foo + Bar (result = 42 + 21 = 63)
testTreeAdditiondefine2 :: Tree
testTreeAdditiondefine2 = Node "+" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Symbol "bar")))

main :: IO ()
main = computeASTs (Env [])
    [   
        testTreeDefine,
        testTreeDefine2,
        testTreeAddition,
        testTreeAdditiondefine,
        testTreeAdditiondefine2
    ]
