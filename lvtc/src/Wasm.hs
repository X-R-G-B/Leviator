{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ShuntingYard
-}

module Wasm
(
    VariableType (..)
    , TypeSectionType (..)
    , TypeSection (..)
    , FunctionSection (..)
    , MemorySection (..)
    , ExportSectionExportType (..)
    , ExportSectionExport (..)
    , ExportSection (..)
    , CodeSectionCodeLocals
    , CodeSectionCode (..)
    , CodeSection (..)
    , Wasm (..)
) where

import WatAST (OpCode (..))

data VariableType =
    I32

data TypeSectionType =
    Func {
        headerFunc :: Int,
        nbParams :: Int,
        params :: [VariableType],
        nbResults :: Int,
        results :: [VariableType]
    }

data TypeSection = TS {
    headerTS :: Int,
    sizeTS :: Int,
    nbTypes :: Int,
    types :: [TypeSectionType]
}

data FunctionSection = FS {
    headerFS :: Int,
    sizeFS :: Int,
    nbFuncs :: Int,
    funcs :: [Int]
}

data MemorySection = MS {
    headerMS :: Int,
    sizeMS :: Int,
    hasMax :: Int,
    minMS :: Int,
    maxMS :: Int
}

data ExportSectionExportType =
    FuncExport
    | TableExport
    | MemoryExport
    | GlobalExport

data ExportSectionExport = ESE {
    nameLength :: Int,
    name :: String,
    typeESE :: ExportSectionExportType,
    indexESE :: Int
}

data ExportSection = ES {
    headerES :: Int,
    sizeES :: Int,
    nbExports :: Int,
    exports :: [ExportSectionExport]
}

type CodeSectionCodeLocals = (Int, VariableType)

data CodeSectionCode = CSC {
    sizeCSC :: Int,
    nbLocals :: Int,
    locals :: [CodeSectionCodeLocals],
    bodyCSC :: [OpCode],
    endCSC :: Int
}

data CodeSection = CS {
    headerCS :: Int,
    sizeCS :: Int,
    nbCodes :: Int,
    codes :: [CodeSectionCode]
}

data Wasm = Wasm {
    headerWasm :: (Int, Int, Int, Int),
    versionWasm :: (Int, Int, Int, Int),
    typeSection :: TypeSection,
    functionSection :: FunctionSection,
    memorySection :: MemorySection,
    exportSection :: ExportSection,
    codeSection :: CodeSection
}
