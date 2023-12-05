{-
-- EPITECH PROJECT, 2023
<<<<<<< HEAD
-- Koaky
=======
-- Main
>>>>>>> main
-- File description:
-- Main
-}

import ComputeAST
import AST
import Defines

-- Foo = 42
test1 :: Tree
test1 = Node "define" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 42)))

-- Bar = 21
test2 :: Tree
test2 = Node "define" (Just (Leaf (Symbol "bar"))) (Just (Leaf (Number 21)))

-- 21 + 21 (result = 42)
test3 :: Tree
test3 = Node "+" (Just (Leaf (Number 21))) (Just (Leaf (Number 21)))

-- Foo + 42 (result = 42 + 42 = 84)
test4 :: Tree
test4 = Node "+" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 42)))

-- Foo + Bar (result = 42 + 21 = 63)
test5 :: Tree
test5 = Node "+" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Symbol "bar")))

-- 2 + (5 * 2) (result = 12)
test6 :: Tree
test6 = Node "+" (Just (Leaf (Number 2))) (Just (Node "*" (Just (Leaf (Number 5))) (Just (Leaf (Number 2)))))

-- 2 + (foo / 2) (result = 23)
test7 :: Tree
test7 = Node "+" (Just (Leaf (Number 2))) (Just (Node "/" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 2)))))

-- 2 + 2 + (5 * 2) (result = 14)
test8 :: Tree
test8 = Node "+" (Just (Leaf (Number 2))) (Just (Node "+" (Just (Leaf (Number 2))) (Just (Node "*" (Just (Leaf (Number 5))) (Just (Leaf (Number 2)))))))

-- (2 * 5) + (foo / 2) (result = 10 + 21 = 31)
test9 :: Tree
test9 = Node "+" (Just (Node "*" (Just (Leaf (Number 2))) (Just (Leaf (Number 5))))) (Just (Node "/" (Just (Leaf (Symbol "foo"))) (Just (Leaf (Number 2)))))

main :: IO ()
main = computeAllAST (Env [])
    [   
        test1,
        test2,
        test3,
        test4,
        test5,
        test6,
        test7,
        test8,
        test9
    ]
