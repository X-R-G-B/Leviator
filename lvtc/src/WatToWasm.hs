{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- WatToWasm
-}

module WatToWasm
(
    watToWasm
) where

import Wasm
import WasmUtils
import WatAST

import Data.List (sort)

watASTTypeToWasm :: Type -> VariableType
watASTTypeToWasm WatAST.I32 = Wasm.I32

-- Type Section

addFuncToType :: FuncDef -> [([Type], Type)] -> [([Type], Type)]
addFuncToType (FuncDef _ _ _ p r _ _) [] = [(p, r)]
addFuncToType (FuncDef i o n p r b l) ((p', r'):xs)
    | p == p' && r == r' = (p', r') : xs
    | otherwise = (p', r') : addFuncToType (FuncDef i o n p r b l) xs

funcDefsToTypeSectionType' :: [FuncDef] -> [([Type], Type)] -> [([Type], Type)]
funcDefsToTypeSectionType' [] tSection = tSection
funcDefsToTypeSectionType' (x:xs) tSection =
    funcDefsToTypeSectionType' xs tSection'
    where
        tSection' = addFuncToType x tSection

toTypeSectionType :: ([Type], Type) -> TypeSectionType
toTypeSectionType (p, r) =
    fillBlankTypeSectionType (getDefaultTypeSectionType {
        params = map watASTTypeToWasm p,
        results = [watASTTypeToWasm r]
    })

funcDefsToTypeSectionType :: [FuncDef] -> [TypeSectionType]
funcDefsToTypeSectionType fs = map toTypeSectionType tSection
    where
        tSection = funcDefsToTypeSectionType' fs []

funcDefsToTypeSection :: [FuncDef] -> TypeSection
funcDefsToTypeSection f =
    fillBlankTypeSection (getDefaultTypeSection {
        types = funcDefsToTypeSectionType f
    })

-- Function Section

getIndexType :: [([Type], Type)] -> Int -> FuncDef -> Int
getIndexType [] _ (FuncDef _ oName _ _ _ _ _) =
    error ("No Type in type section: " ++ oName)
getIndexType ((p', r'):xs) ind (FuncDef i o n p r b l)
    | p == p' && r == r' = ind
    | otherwise = getIndexType xs (ind + 1) (FuncDef i o n p r b l) 

funcDefsToFunctionSection :: [FuncDef] -> FunctionSection
funcDefsToFunctionSection fs =
    fillBlankFunctionSection (getDefaultFunctionSection {
        funcs = map (getIndexType tSection 0) fs
    })
    where
        tSection = funcDefsToTypeSectionType' fs []

-- Memory Section

funcDefsToMemorySection :: [FuncDef] -> MemorySection
funcDefsToMemorySection _ =
    fillBlankMemorySection (getDefaultMemorySection {
        hasMax = 0,
        minMS = 1
    })

-- Export Section

funcDefsToExportSectionExport :: [FuncDef] -> [ExportSectionExport]
funcDefsToExportSectionExport [] = []
funcDefsToExportSectionExport ((FuncDef True oName oInd _ _ _ _):xs) =
    fillBlankExportSectionExport (ESE {
        nameLength = 0,
        name = oName,
        typeESE = FuncExport,
        indexESE = read (show oInd) :: Int
    }) : funcDefsToExportSectionExport xs
funcDefsToExportSectionExport (_:xs) = funcDefsToExportSectionExport xs

funcDefsToExportSection :: [FuncDef] -> ExportSection
funcDefsToExportSection fs =
    fillBlankExportSection (getDefaultExportSection {
        exports = funcDefsToExportSectionExport fs
    })

-- Code Section

funcDefToCodeSectionCode :: FuncDef -> CodeSectionCode
funcDefToCodeSectionCode (FuncDef _ _ _ _ _ b l) =
    fillBlankCodeSectionCode (getDefaultCodeSectionCode {
        locals = map (\(x, y) -> (y, watASTTypeToWasm x)) l,
        bodyCSC = b
    })

funcDefsToCodeSection :: [FuncDef] -> CodeSection
funcDefsToCodeSection f =
    fillBlankCodeSection (getDefaultCodeSection {
        codes = map funcDefToCodeSectionCode f
    })

-- main

sortFuncDef :: [FuncDef] -> [FuncDef]
sortFuncDef = sort

watToWasm :: [FuncDef] -> Wasm
watToWasm fs =
    getDefaultWasm {
        typeSection = funcDefsToTypeSection fsSorted,
        functionSection = funcDefsToFunctionSection fsSorted,
        memorySection = funcDefsToMemorySection fsSorted,
        exportSection = funcDefsToExportSection fsSorted,
        codeSection = funcDefsToCodeSection fsSorted
    }
    where
        fsSorted = sortFuncDef fs
