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
    ,   testCase "basic2" $
            assertEqual "Basic2"
                basic2_rep
                (watsLikeToWat basic2)
    ]
    where
        basic1 =
            builtinsWatLike ++
            [
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
        basic1_rep =
            getBuiltinsWat ++
            [
                FuncDef 10 [] I32 [
                    I32Const 97,
                    LocalSet 0,
                    LocalGet 0,
                    WatAST.Return
                ] [(I32, 1)]
            ]
        basic2 =
            builtinsWatLike ++
            [
                (
                    (
                        ("10", [("0", "Int"), ("1", "Int")], "Int"),
                        [
                            Declaration (("2", "Int"), FuncValue ("0", [Var "0", Var "1"])),
                            AST.Return (Var "2")
                        ]
                    ),
                    [(0, "a"), (1, "b"), (2, "_tmpValue")]
                ),
                (
                    (
                        ("11", [], "Int"),
                        [
                            Declaration (("0", "Int"), Integer 1),
                            Declaration (("1", "Int"), Integer 2),
                            Declaration (("2", "Int"), FuncValue ("10", [Var "0", Var "1"])),
                            AST.Return (Var "2")
                        ]
                    ),
                    [(0, "_tmpValue"), (1, "_tmpValue"), (2, "_tmpValue")]
                )
            ]
        basic2_rep =
            getBuiltinsWat ++
            [
                FuncDef 10 [I32, I32] I32 [
                    LocalGet 0,
                    LocalGet 1,
                    Call 0,
                    LocalSet 2,
                    LocalGet 2,
                    WatAST.Return
                ] [(I32, 1)],
                FuncDef 11 [] I32 [
                    I32Const 1,
                    LocalSet 0,
                    I32Const 2,
                    LocalSet 1,
                    LocalGet 0,
                    LocalGet 1,
                    Call 10,
                    LocalSet 2,
                    LocalGet 2,
                    WatAST.Return
                ] [(I32, 3)]
            ]
