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
test1 :: Tree
test1 = Node "define" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 42)))

-- 21 + 21 (result = 42)
test2 :: Tree
test2 = Node "+" (Just (Leaf (Number 21))) (Just (Leaf (Number 21)))

-- bool = true
test3 :: Tree
test3 = Leaf (Boolean True)

-- bool = false
test4 :: Tree
test4 = Leaf (Boolean False)

main :: IO ()
main = print $ computeAllAST (Env []) [test1, test2, test3, test4]
