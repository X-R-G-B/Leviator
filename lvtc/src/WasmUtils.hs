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
    fillBlankMemorySectionLimits,
    fillBlankMemorySection,
    fillBlankExportSectionExport,
    fillBlankExportSection,
    fillBlankCodeSectionCode,
    fillBlankCodeSection,
    --
    opCodeByte,
    variableTypeByte,
    exportSectionExportTypeByte,
    ifTypeByte
) where

import Wasm
import WatAST (OpCode (..), IfType (..))
import Leb128Encode

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
    nbLimits = 0,
    limits = []
}

fillBlankMemorySectionLimits :: MemorySectionLimits -> MemorySectionLimits
fillBlankMemorySectionLimits (MSL 0 miMS maMS) =
    MSL {
        hasMax = 0,
        minMS = miMS,
        maxMS = maMS
    }
fillBlankMemorySectionLimits (MSL _ miMS maMS) =
    MSL {
        hasMax = 1,
        minMS = miMS,
        maxMS = maMS
    }

getSizeMemorySectionLimits :: MemorySectionLimits -> Int
getSizeMemorySectionLimits (MSL 0 _ _) = 1 + 1
getSizeMemorySectionLimits (MSL _ _ _) = 1 + 1 + 1

fillBlankMemorySection :: MemorySection -> MemorySection
fillBlankMemorySection (MS hM _ _ m) =
    MS {
        headerMS = hM,
        sizeMS = size,
        nbLimits = length m,
        limits = m
    }
    where
        size = 1 + sum (map getSizeMemorySectionLimits m)

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
    (length (leb128Encode (length n))) + (length n) + 1 + 1

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
        size = 1 + sum (map (getSizeExportSectionExport) e)

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
getSizeOpCode (If _) = 2
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
getSizeCodeSectionCode csc = lN + s
    where
        newCsc = fillBlankCodeSectionCode csc
        s = sizeCSC newCsc
        lN = length (leb128Encode s)

fillBlankCodeSection :: CodeSection -> CodeSection
fillBlankCodeSection (CS hC _ _ c) =
    CS {
        headerCS = hC,
        sizeCS = size,
        nbCodes = length c,
        codes = c
    }
    where
        size1 = sum (map getSizeCodeSectionCode c)
        size = size1 + 1

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

opCodeByte :: OpCode -> Int
opCodeByte (LocalGet _) = 0x20
opCodeByte (LocalSet _) = 0x21
opCodeByte (I32Const _) = 0x41
opCodeByte I32Store = 0x36
opCodeByte I32Load = 0x28
opCodeByte I32GT_S = 0x4a
opCodeByte I32LT_S = 0x48
opCodeByte I32GE_S = 0x4e
opCodeByte I32LE_S = 0x4c
opCodeByte I32EQ = 0x46
opCodeByte I32NE = 0x47
opCodeByte I32Add = 0x6a
opCodeByte I32Sub = 0x6b
opCodeByte I32Mul = 0x6c
opCodeByte I32Div = 0x6d
opCodeByte Return = 0x0f
opCodeByte (Call _) = 0x10
opCodeByte (If EmptyType) = 0x04
opCodeByte Else = 0x05
opCodeByte End = 0x0b

ifTypeByte :: IfType -> Int
ifTypeByte EmptyType = 0x40

variableTypeByte :: VariableType -> Int
variableTypeByte I32 = 0x7f

exportSectionExportTypeByte :: ExportSectionExportType -> Int
exportSectionExportTypeByte (FuncExport) = 0x00
exportSectionExportTypeByte (TableExport) = 0x01
exportSectionExportTypeByte (MemoryExport) = 0x02
exportSectionExportTypeByte (GlobalExport) = 0x03
