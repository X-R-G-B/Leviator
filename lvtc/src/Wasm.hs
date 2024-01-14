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
    , MemorySectionLimits (..)
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

pad :: String -> String
pad [] = []
pad ('\n':xs) = '\n':' ':' ' : pad xs
pad (x:xs) = x : pad xs

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
    deriving (Eq)

instance Show TypeSectionType where
    show (Func hF nP p nR r) =
        "Func {\n"
        ++ "  headerFunc: " ++ show hF ++ "\n"
        ++ "  nbParams: " ++ show nP ++ "\n"
        ++ "  params: " ++ show p ++ "\n"
        ++ "  nbResults: " ++ show nR ++ "\n"
        ++ "  results: " ++ show r ++ "\n"
        ++ "}"

data TypeSection =
    TS {
        headerTS :: Int,
        sizeTS :: Int,
        nbTypes :: Int,
        types :: [TypeSectionType]
    }
    deriving (Eq)

instance Show TypeSection where
    show (TS hT sT nT t) =
        "TS {\n"
        ++ "  headerTS: " ++ show hT ++ "\n"
        ++ "  sizeTS: " ++ show sT ++ "\n"
        ++ "  nbTypes: " ++ show nT ++ "\n"
        ++ "  types: " ++ pad (pad (show t)) ++ "\n"
        ++ "}"

data FunctionSection =
    FS {
        headerFS :: Int,
        sizeFS :: Int,
        nbFuncs :: Int,
        funcs :: [Int]
    }
    deriving (Eq)

instance Show FunctionSection where
    show (FS hF sF nF f) =
        "FS {\n"
        ++ "  headerFS: " ++ show hF ++ "\n"
        ++ "  sizeFS: " ++ show sF ++ "\n"
        ++ "  nbFuncs: " ++ show nF ++ "\n"
        ++ "  funcs: " ++ pad (pad (show f)) ++ "\n"
        ++ "}"

data MemorySectionLimits =
    MSL {
        hasMax :: Int,
        minMS :: Int,
        maxMS :: Int
    }
    deriving (Eq)

instance Show MemorySectionLimits where
    show (MSL hM mMi mMa) =
        "MSL {\n"
        ++ "  hasMax: " ++ show hM ++ "\n"
        ++ "  minMS: " ++ show mMi ++ "\n"
        ++ "  maxMS: " ++ show mMa ++ "\n"
        ++ "}"

data MemorySection =
    MS {
        headerMS :: Int,
        sizeMS :: Int,
        nbLimits :: Int,
        limits :: [MemorySectionLimits]
    }
    deriving (Eq)

instance Show MemorySection where
    show (MS hM sM nL l) =
        "MS {\n"
        ++ "  headerMS: " ++ show hM ++ "\n"
        ++ "  sizeMS: " ++ show sM ++ "\n"
        ++ "  nbLimits: " ++ show nL ++ "\n"
        ++ "  limits: " ++ pad (pad (show l)) ++ "\n"
        ++ "}"

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
    deriving (Eq)

instance Show ExportSectionExport where
    show (ESE nL n t i) =
        "ESE {\n"
        ++ "  nameLength: " ++ show nL ++ "\n"
        ++ "  name: " ++ show n ++ "\n"
        ++ "  typeESE: " ++ show t ++ "\n"
        ++ "  indexESE: " ++ show i ++ "\n"
        ++ "}"

data ExportSection =
    ES {
        headerES :: Int,
        sizeES :: Int,
        nbExports :: Int,
        exports :: [ExportSectionExport]
    }
    deriving (Eq)

instance Show ExportSection where
    show (ES hE sE nE e) =
        "ES {\n"
        ++ "  headerES: " ++ show hE ++ "\n"
        ++ "  sizeES: " ++ show sE ++ "\n"
        ++ "  nbExports: " ++ show nE ++ "\n"
        ++ "  exports: " ++ pad (pad (show e)) ++ "\n"
        ++ "}"

type CodeSectionCodeLocals = (Int32, VariableType)

data CodeSectionCode =
    CSC {
        sizeCSC :: Int,
        nbLocals :: Int,
        locals :: [CodeSectionCodeLocals],
        bodyCSC :: [OpCode],
        endCSC :: Int
    }
    deriving (Eq)

instance Show CodeSectionCode where
    show (CSC sL nL l b e) =
        "CSC {\n"
        ++ "  sizeCSC: " ++ show sL ++ "\n"
        ++ "  nbLocals: " ++ show nL ++ "\n"
        ++ "  locals: " ++ pad (pad (show l)) ++ "\n"
        ++ "  bodyCSC: " ++ pad (pad (show b)) ++ "\n"
        ++ "  endCSC: " ++ show e ++ "\n"
        ++ "}"

data CodeSection =
    CS {
        headerCS :: Int,
        sizeCS :: Int,
        nbCodes :: Int,
        codes :: [CodeSectionCode]
    }
    deriving (Eq)

instance Show CodeSection where
    show (CS hC sC nC c) =
        "CS {\n"
        ++ "  headerCS: " ++ show hC ++ "\n"
        ++ "  sizeCS: " ++ show sC ++ "\n"
        ++ "  nbCodes: " ++ show nC ++ "\n"
        ++ "  codes: " ++ pad (pad (show c)) ++ "\n"
        ++ "}"

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
    deriving (Eq)

instance Show Wasm where
    show (Wasm h v t f m e c) =
        "Wasm {\n"
        ++ "  headerWasm: " ++ pad (show h) ++ "\n"
        ++ "  versionWasm: " ++ pad (show v) ++ "\n"
        ++ "  typeSection: " ++ pad (show t) ++ "\n"
        ++ "  functionSection: " ++ pad (show f) ++ "\n"
        ++ "  memorySection: " ++ pad (show m) ++ "\n"
        ++ "  exportSection: " ++ pad (show e) ++ "\n"
        ++ "  codeSection: " ++ pad (show c) ++ "\n"
        ++ "}"
