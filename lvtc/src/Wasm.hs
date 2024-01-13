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
import Data.Int (Int32)

data VariableType =
    I32
    deriving (Show, Eq)

data TypeSectionType =
    Func {
        headerFunc :: Int,
        nbParams :: Int,
        params :: [VariableType],
        nbResults :: Int,
        results :: [VariableType]
    }
    deriving (Show, Eq)

data TypeSection =
    TS {
        headerTS :: Int,
        sizeTS :: Int,
        nbTypes :: Int,
        types :: [TypeSectionType]
    }
    deriving (Show, Eq)

data FunctionSection =
    FS {
        headerFS :: Int,
        sizeFS :: Int,
        nbFuncs :: Int,
        funcs :: [Int]
    }
    deriving (Show, Eq)

data MemorySection =
    MS {
        headerMS :: Int,
        sizeMS :: Int,
        hasMax :: Int,
        minMS :: Int,
        maxMS :: Int
    }
    deriving (Show, Eq)

data ExportSectionExportType =
    FuncExport
    | TableExport
    | MemoryExport
    | GlobalExport
    deriving (Show, Eq)

data ExportSectionExport =
    ESE {
        nameLength :: Int,
        name :: String,
        typeESE :: ExportSectionExportType,
        indexESE :: Int
    }
    deriving (Show, Eq)

data ExportSection =
    ES {
        headerES :: Int,
        sizeES :: Int,
        nbExports :: Int,
        exports :: [ExportSectionExport]
    }
    deriving (Show, Eq)

type CodeSectionCodeLocals = (Int32, VariableType)

data CodeSectionCode =
    CSC {
        sizeCSC :: Int,
        nbLocals :: Int,
        locals :: [CodeSectionCodeLocals],
        bodyCSC :: [OpCode],
        endCSC :: Int
    }
    deriving (Show, Eq)

data CodeSection =
    CS {
        headerCS :: Int,
        sizeCS :: Int,
        nbCodes :: Int,
        codes :: [CodeSectionCode]
    }
    deriving (Show, Eq)

data Wasm =
    Wasm {
        headerWasm :: (Int, Int, Int, Int),
        versionWasm :: (Int, Int, Int, Int),
        typeSection :: TypeSection,
        functionSection :: FunctionSection,
        memorySection :: MemorySection,
        exportSection :: ExportSection,
        codeSection :: CodeSection
    }
    deriving (Show, Eq)
