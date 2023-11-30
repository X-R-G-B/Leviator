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

-- Define = <SYMBOL> <EXPRESSION>

data Define = Define {
    symbol :: String,
    expression :: Atom
} deriving (Show)

type Defines = [Define]

-- Function to print Defines if needed (debugging)
printDefines :: Defines -> IO ()
printDefines [] = return ()
printDefines (x:xs) = putStrLn output >> printDefines xs
    where output =
            ("Symbol: " ++ (symbol x) ++
            " Expr: " ++ (show (expression x)))

-- Register a define in the Defines list
registerDefine :: Defines -> Tree -> Defines
registerDefine defines
        (Node "define"
        (Just (Leaf (Symbol defSymbol)))
        (Just (Leaf defexpression)))
        = defines ++ [Define defSymbol defexpression]
registerDefine defines _ = defines

-- Temporary function to compute defines only
-- Will be replaced by a function to compute the whole AST
computeAST :: Tree -> IO ()
computeAST tree = do
    let defines = registerDefine [] tree
    printDefines defines
    return ()
