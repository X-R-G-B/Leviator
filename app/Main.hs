{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import ComputeAST
import AST
import Defines

testTreeDefine :: Tree
-- data Tree = Node Symbol (Maybe Tree) (Maybe Tree) | Leaf Atom
testTreeDefine = Node "define" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 21)))

testTreeDefine2 :: Tree
testTreeDefine2 = Node "define" (Just (Leaf (Symbol "bar"))) (Just (Leaf (Number 42)))

main :: IO ()
main = computeASTs [testTreeDefine, testTreeDefine2]
