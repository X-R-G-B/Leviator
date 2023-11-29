{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import ComputeAST
import AST

createDefineTestTree :: Tree
-- data Tree = Node Symbol (Maybe Tree) (Maybe Tree) | Leaf Atom
createDefineTestTree = Node "define" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 21)))

main :: IO ()
main = computeAST createDefineTestTree
