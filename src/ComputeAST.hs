{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module ComputeAST
    (
        computeAST
    ) where

import AST

--Defines :
-- <SYMBOL> <EXPRESSION> with <EXPRESSION> being a type Atom or Symbol
--foo 21
--func add (lambda (a b) (+ a b))

type Expression = Atom

data Define = Define {
    symbol :: String,
    value :: Expression
}

type Defines = [Define]

print

handleDefine :: Tree -> IO ()
handleDefine (Node "define" (Just (Leaf (Symbol symbol))) (Just (Leaf value))) = putStrLn ("defining: " ++ symbol ++ " as " ++ show value)
handleDefine _ = putStrLn "Error: Invalid AST"

--data Tree = Node Symbol Tree Tree | Leaf Atom

computeAST :: Tree -> IO ()
computeAST (Node "define" (Just (Leaf (Symbol symbol))) (Just value)) = handleDefine (Node "define" (Just (Leaf (Symbol symbol))) (Just value))
computeAST _ = putStrLn "Error: Invalid AST"
