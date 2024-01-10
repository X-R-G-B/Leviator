{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ShuntingYard
-}

module WasmUtils
(
    getDefaultTypeSectionType,
    getDefaultTypeSection,
    getDefaultFunctionSection,
    getDefaultMemorySection,
    getDefaultExportSection,
    getDefaultCodeSectionCode,
    getDefaultCodeSection,
    getDefaultWasm,
    --
    fillBlankTypeSectionType,
    fillBlankTypeSection,
    fillBlankFunctionSection,
    fillBlankMemorySection,
    fillBlankExportSectionExport,
    fillBlankExportSection,
    fillBlankCodeSectionCode,
    fillBlankCodeSection,
) where

import Wasm
import WatAST (OpCode (..))

getDefaultTypeSectionType :: TypeSectionType
getDefaultTypeSectionType = Func {
    headerFunc = 0x60,
    nbParams = 0x0,
    params = [],
    nbResults = 0,
    results = []
}

fillBlankTypeSectionType :: TypeSectionType -> TypeSectionType
fillBlankTypeSectionType (Func hF _ p _ r) =
    Func {
        headerFunc = hF,
        nbParams = length p,
        params = p,
        nbResults = length r,
        results = r
    }

getSizeTypeSectionType :: TypeSectionType -> Int
getSizeTypeSectionType (Func _ _ p _ r) =
    1 + 1 + ((length p) * 1) + 1 + ((length r) * 1)

getDefaultTypeSection :: TypeSection
getDefaultTypeSection = TS {
    headerTS = 0x01,
    sizeTS = 0x0,
    nbTypes = 0,
    types = []
}

fillBlankTypeSection :: TypeSection -> TypeSection
fillBlankTypeSection (TS hT _ _ t) =
    TS {
        headerTS = hT,
        sizeTS = size,
        nbTypes = length t,
        types = t
    }
    where
        size = 1 + sum (map getSizeTypeSectionType t)

getDefaultFunctionSection :: FunctionSection
getDefaultFunctionSection = FS {
    headerFS = 0x03,
    sizeFS = 0x0,
    nbFuncs = 0,
    funcs = []
}

fillBlankFunctionSection :: FunctionSection -> FunctionSection
fillBlankFunctionSection (FS hF _ _ f) =
    FS {
        headerFS = hF,
        sizeFS = size,
        nbFuncs = length f,
        funcs = f
    }
    where
        size = 1 + ((length f) * 1)

getDefaultMemorySection :: MemorySection
getDefaultMemorySection = MS {
    headerMS = 0x05,
    sizeMS = 0x0,
    hasMax = 0x0,
    minMS = 0x0,
    maxMS = 0x0
}

fillBlankMemorySection :: MemorySection -> MemorySection
fillBlankMemorySection (MS hM _ 0 miMS maMS) =
    MS {
        headerMS = hM,
        sizeMS = size,
        hasMax = 0,
        minMS = miMS,
        maxMS = maMS
    }
    where
        size = 2
fillBlankMemorySection (MS hM _ _ miMS maMS) =
    MS {
        headerMS = hM,
        sizeMS = size,
        hasMax = 1,
        minMS = miMS,
        maxMS = maMS
    }
    where
        size = 3

fillBlankExportSectionExport :: ExportSectionExport -> ExportSectionExport
fillBlankExportSectionExport (ESE _ n t i) =
    ESE {
        nameLength = length n,
        name = n,
        typeESE = t,
        indexESE = i
    }

getSizeExportSectionExport :: ExportSectionExport -> Int
getSizeExportSectionExport (ESE _ n _ _) =
    1 + ((length n) * 1) + 1 + 1

getDefaultExportSection :: ExportSection
getDefaultExportSection = ES {
    headerES = 0x07,
    sizeES = 0x0,
    nbExports = 0,
    exports = []
}

fillBlankExportSection :: ExportSection -> ExportSection
fillBlankExportSection (ES hE _ _ e) =
    ES {
        headerES = hE,
        sizeES = size,
        nbExports = length e,
        exports = e
    }
    where
        size = 1 + sum (map getSizeExportSectionExport e)

getDefaultCodeSectionCode :: CodeSectionCode
getDefaultCodeSectionCode = CSC {
    sizeCSC = 0x0,
    nbLocals = 0,
    locals = [],
    bodyCSC = [],
    endCSC = 0x0b
}

getSizeOpCode :: OpCode -> Int
getSizeOpCode (LocalGet _) = 2
getSizeOpCode (LocalSet _) = 2
getSizeOpCode (I32Const _) = 2
getSizeOpCode (Call _) = 2
getSizeOpCode _ = 1

fillBlankCodeSectionCode :: CodeSectionCode -> CodeSectionCode
fillBlankCodeSectionCode (CSC _ _ l b e) =
    CSC {
        sizeCSC = size,
        nbLocals = length l,
        locals = l,
        bodyCSC = b,
        endCSC = e
    }
    where
        size = 1 + ((length l) * 2) + (sum (map getSizeOpCode b)) + 1

getDefaultCodeSection :: CodeSection
getDefaultCodeSection = CS {
    headerCS = 0x0a,
    sizeCS = 0x0,
    nbCodes = 0,
    codes = []
}

getSizeCodeSectionCode :: CodeSectionCode -> Int
getSizeCodeSectionCode (CSC s _ _ _ _) =
    1 + s

fillBlankCodeSection :: CodeSection -> CodeSection
fillBlankCodeSection (CS hC _ _ c) =
    CS {
        headerCS = hC,
        sizeCS = size,
        nbCodes = length c,
        codes = c
    }
    where
        size = 1 + sum (map getSizeCodeSectionCode c)

getDefaultWasm :: Wasm
getDefaultWasm = Wasm {
    headerWasm = (0x00, 0x61, 0x73, 0x6d),
    versionWasm = (0x01, 0x00, 0x00, 0x00),
    typeSection = getDefaultTypeSection,
    functionSection = getDefaultFunctionSection,
    memorySection = getDefaultMemorySection,
    exportSection = getDefaultExportSection,
    codeSection = getDefaultCodeSection
}
