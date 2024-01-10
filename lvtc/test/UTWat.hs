{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- UTWatLike
-}

module UTWat
(
    utWat
) where

import Test.Tasty
import Test.Tasty.HUnit

import WatAST
import AST
import WatLikeToWat
import Builtins
import UTWatLike

utWat :: TestTree
utWat = testGroup "Wat"
    [
        testCase "basic" $
            assertEqual "Basic"
                basic1_rep
                (watsLikeToWat basic1)
    ]
    where
        basic1 = builtinsWatLike ++ [
                (
                    (
                        ("10", [], "Int"),
                        [
                            Declaration (("0", "Int"), Integer 97),
                            AST.Return (Var "0")
                        ]
                    ),
                    [(0, "_tmpValue")]
                )
            ]
        basic1_rep = getBuiltinsWat ++ [
                FuncDef 10 [] I32 [
                    I32Const 97,
                    LocalSet 0,
                    LocalGet 0,
                    WatAST.Return
                ] [(I32, 1)]
            ]
