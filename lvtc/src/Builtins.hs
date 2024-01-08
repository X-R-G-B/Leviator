{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Builtins
-}

module Builtins
(
    getBuiltinsFunc
) where

import AST

getBuiltinsFunc :: [FuncDeclaration]
getBuiltinsFunc =
    [
        (("+", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("-", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("*", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("/", [("x", "Int"), ("y", "Int")], "Int"), [])
    ]
