{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import ComputeAST
import Defines
import AST

-- define x 42
tree1 :: Tree
tree1 = List [Symbol "define", Symbol "x", Number 42]

tree2 :: Tree
tree2 = Symbol "x"

main :: IO ()
main = print $ computeAllAST (Env {defines = [], errors = []}) [tree1, tree2]
