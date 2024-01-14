{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- UTLexeme
-}

module UTLexeme
(
    utLexeme
) where

import Test.Tasty
import Test.Tasty.HUnit

import Lexeme

utLexeme :: TestTree
utLexeme = testGroup "UTLexeme"
    [
        testCase "lexeme1" $
            assertEqual "1"
                l1_rep
                (lexeme1 l1)
    ,   testCase "lexeme2" $
            assertEqual "2"
                l2_rep
                (lexeme1 l2)
    ,   testCase "lexeme3" $
            assertEqual "3"
                l3_rep
                (lexeme1 l3)
    ,   testCase "lexeme4" $
            assertEqual "4"
                l4_rep
                (lexeme1 l4)
    ,   testCase "lexeme5" $
            assertEqual "5"
                l5_rep
                (lexeme1 l5)
    ,   testCase "lexeme6" $
            assertEqual "6"
                l6_rep
                (lexeme1 l6)
    ]
    where
        l1 = "@Int a = 0;"
        l1_rep = "@Int a=0;"
        l2 = "if (a > b) {\n    do(b);\n}\n"
        l2_rep = "if(a>b){do(b);}"
        l3 = "if (a)\n{\nb(0);\n}\nelse\n{\nc(0);\n};\n"
        l3_rep = "if(a){b(0);}else{c(0);};"
        l4 = "@Int a = 0;\n    @Int c = b(a);\n    if (c)\n    {\n        d(a);\n    };\n"
        l4_rep = "@Int a=0;@Int c=b(a);if(c){d(a);};"
        l5 = "foo(a);\n    foo(b);\n"
        l5_rep = "foo(a);foo(b);"
        l6 = "export fn start() -> Int\n{\n    @Int one = 15;\n    @Int two = 5;\n    three = 0;\n    if (one == 5)\n    {\n        three = 15;\n    };\n    if (two == 5)\n{\n        three = 5;\n    };\n    <- three;\n};\n"
        l6_rep = "export fn start()->Int{@Int one=15;@Int two=5;three=0;if(one==5){three=15;};if(two==5){three=5;};<-three;};"
