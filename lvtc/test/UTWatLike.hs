{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- UTWatLike
-}

module UTWatLike (
    utWatLike
    , builtinsWatLike
) where

import Test.Tasty
import Test.Tasty.HUnit

import WatLike
import AST

builtinsWatLike :: [FuncDeclare]
builtinsWatLike = 
    [
        (
            (("0", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("1", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("2", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("3", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("4", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("5", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("6", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("7", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("8", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        ),
        (
            (("9", [("0", "Int"), ("1", "Int")], "Int"), []),
            [(0, "x"), (1, "y")]
        )
    ]

utWatLike :: TestTree
utWatLike = testGroup "Wat Like"
  [
    testCase "basic" $
      assertEqual "Basic"
        basic1_rep
        (aSTToWatLike [basic1])
  , testCase "basic basic" $
      assertEqual "Basic Basic"
        basic2_rep
        (aSTToWatLike basic2)
  , testCase "basic basic basic" $
      assertEqual "Basic Basic Basic"
        basic3_rep
        (aSTToWatLike [basic3])
  ]
  where
    basic1 = 
        (
            ("add", [("a", "Int"), ("b", "Int")], "Int"),
            [Return (FuncValue ("+", [Var "a", Var "b"]))]
        )
    basic1_rep =
        builtinsWatLike ++ [
            (
                (
                    ("10", [("0", "Int"), ("1", "Int")], "Int"),
                    [
                        Declaration (("2", "Int"), FuncValue ("0", [Var "0", Var "1"])),
                        Return (Var "2")
                    ]
                ),
                [(0, "a"), (1, "b"), (2, "_tmpValue")]
            )
        ]
    basic2 =
        [
            (
                ("add", [("a", "Int"), ("b", "Int")], "Int"),
                [Return (FuncValue ("+", [Var "a", Var "b"]))]
            ),
            (
                ("start", [], "Int"),
                [Return (FuncValue ("add", [Integer 1, Integer 2]))]
            )
        ]
    basic2_rep =
        builtinsWatLike ++ [
            (
                (
                    ("10", [("0", "Int"), ("1", "Int")], "Int"),
                    [
                        Declaration (("2", "Int"), FuncValue ("0", [Var "0", Var "1"])),
                        Return (Var "2")
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
                        Return (Var "2")
                    ]
                ),
                [(0, "_tmpValue"), (1, "_tmpValue"), (2, "_tmpValue")]
            )
        ]
    basic3 = 
        (
            ("getC", [], "Char"),
            [Return (Character 'a')]
        )
    basic3_rep =
        builtinsWatLike ++ [
            (
                (
                    ("10", [], "Int"),
                    [
                        Declaration (("0", "Int"), Integer 97),
                        Return (Var "0")
                    ]
                ),
                [(0, "_tmpValue")]
            )
        ]
