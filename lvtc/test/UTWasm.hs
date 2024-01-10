{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- UTWasm
-}

module UTWasm
(
    utWasm
) where

import Test.Tasty
import Test.Tasty.HUnit

import Builtins
import Wasm
import WatAST
import WatToWasm

basic1Rep :: Wasm
basic1Rep =
    Wasm {
        headerWasm = (0,97,115,109),
        versionWasm = (1,0,0,0),
        typeSection = TS {
            headerTS = 1,
            sizeTS = 11,
            nbTypes = 2,
            types = [
                Func {
                    headerFunc = 96,
                    nbParams = 2,
                    params = [Wasm.I32,Wasm.I32],
                    nbResults = 1,
                    results = [Wasm.I32]
                },
                Func {
                    headerFunc = 96,
                    nbParams = 0,
                    params = [],
                    nbResults = 1,
                    results = [Wasm.I32]
                }
            ]
        },
        functionSection = FS {
            headerFS = 3,
            sizeFS = 12,
            nbFuncs = 11,
            funcs = [
                0,0,0,0,0,0,0,0,0,0,1
            ]
        },
        memorySection = MS {
            headerMS = 5,
            sizeMS = 2,
            hasMax = 0,
            minMS = 1,
            maxMS = 0
        }, exportSection = ES {
            headerES = 7,
            sizeES = 1,
            nbExports = 0,
            exports = []
        },
        codeSection = CS {
            headerCS = 10,
            sizeCS = 103,
            nbCodes = 11,
            codes = [
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0,
                    locals = [],
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32Add,Return
                    ],
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0,
                    locals = [],
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32Sub,Return
                    ],
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0,
                    locals = [], 
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32Mul,Return
                    ],
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0,
                    locals = [], 
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32Div,Return
                    ],
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0,
                    locals = [],
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32EQ,Return
                    ],
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0, 
                    locals = [], 
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32LT_S,Return
                    ], 
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0 ,
                    locals = [], 
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32GT_S,Return
                    ], 
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8, 
                    nbLocals = 0,
                    locals = [],
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32LE_S,Return
                    ], 
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8, 
                    nbLocals = 0,
                    locals = [],
                    bodyCSC = [
                        LocalGet 0,LocalGet 1, I32GE_S,Return
                    ],
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 8,
                    nbLocals = 0,
                    locals = [],
                    bodyCSC = [
                        LocalGet 0,LocalGet 1,I32NE,Return
                    ],
                    endCSC = 11
                },
                CSC {
                    sizeCSC = 11,
                    nbLocals = 1,
                    locals = [(1,Wasm.I32)],
                    bodyCSC = [
                        I32Const 97,LocalSet 0,LocalGet 0,Return
                    ],
                    endCSC = 11
                }
            ]
        }
    }

utWasm :: TestTree
utWasm = testGroup "Wasm"
    [
        testCase "basic" $
            assertEqual "Basic"
                basic1Rep
                (watToWasm basic1)
    ]
    where
        basic1 =
            getBuiltinsWat ++
            [
                FuncDef False "add" 10 [] WatAST.I32 [
                    I32Const 97,
                    LocalSet 0,
                    LocalGet 0,
                    Return
                ] [(WatAST.I32, 1)]
            ]
