{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- WriteWasm
-}

module WriteWasm
(
    writeWasm
    , wasmToByteString
) where

import Wasm
import WasmUtils
import Leb128Encode
import qualified Data.ByteString as B
import WatAST (OpCode (..))
import Data.Char (ord)

-- extend

extendBytes :: B.ByteString -> [B.ByteString] -> B.ByteString
extendBytes = foldl B.append

extendsBytes :: B.ByteString -> [[B.ByteString]] -> B.ByteString
extendsBytes = foldl extendBytes

--

headerWasmToByteString :: Wasm -> B.ByteString
headerWasmToByteString (Wasm (a, b, c, d) (e, f, g, h) _ _ _ _ _) =
    B.pack (map fromIntegral [a, b, c, d, e, f, g, h])

--

typeSectionTypeToByteString :: TypeSectionType -> B.ByteString
typeSectionTypeToByteString (Func a b lc d le) =
    extendBytes
        (B.pack (map fromIntegral [a, b]))
        [
            B.pack (map (fromIntegral . variableTypeByte) lc),
            B.pack [fromIntegral d],
            B.pack (map (fromIntegral . variableTypeByte) le)
        ]

typeSectionToByteString :: TypeSection -> B.ByteString
typeSectionToByteString (TS a b c ld) =
    extendsBytes
        (B.pack [fromIntegral a])
        [
            [B.pack (map fromIntegral (leb128Encode b))],
            [B.pack [fromIntegral c]],
            map typeSectionTypeToByteString ld
        ]

--

functionSectionToByteString :: FunctionSection -> B.ByteString
functionSectionToByteString (FS a b c ld) =
    extendBytes
        (B.pack [fromIntegral a])
        [
            B.pack (map (fromIntegral) (leb128Encode b)),
            B.pack [fromIntegral c],
            B.pack (map fromIntegral ld)
        ]

--

memorySectionLimitToByteString :: MemorySectionLimits -> B.ByteString
memorySectionLimitToByteString (MSL 0 a _) =
    B.pack [0, fromIntegral a]
memorySectionLimitToByteString (MSL _ a b) =
    B.pack [0, fromIntegral a, fromIntegral b]

memorySectionToByteString :: MemorySection -> B.ByteString
memorySectionToByteString (MS a b c ld) =
    extendsBytes
        (B.pack [fromIntegral a])
        [
            [B.pack (map fromIntegral (leb128Encode b))],
            [B.pack [fromIntegral c]],
            map memorySectionLimitToByteString ld
        ]

--

exportSectionExportToByteString :: ExportSectionExport -> B.ByteString
exportSectionExportToByteString (ESE a lb c d) =
    extendBytes
        (B.pack (map fromIntegral (leb128Encode a)))
        [
            B.pack (map (fromIntegral . ord) lb),
            B.pack [
                fromIntegral (exportSectionExportTypeByte c),
                fromIntegral d
            ]
        ]

exportSectionToByteString :: ExportSection -> B.ByteString
exportSectionToByteString (ES a b c ld) =
    extendsBytes
        (B.pack [fromIntegral a])
        [
            [B.pack (map fromIntegral (leb128Encode b))],
            [B.pack [fromIntegral c]],
            map exportSectionExportToByteString ld
        ]

--

codeSectionCodeLocalsToByte :: CodeSectionCodeLocals -> B.ByteString
codeSectionCodeLocalsToByte (a, b) =
    B.pack [fromIntegral a, fromIntegral (variableTypeByte b)]

opCodeToByte :: OpCode -> B.ByteString
opCodeToByte (LocalGet a) =
    B.pack [fromIntegral (opCodeByte (LocalGet a)), fromIntegral a]
opCodeToByte (LocalSet a) =
    B.pack [fromIntegral (opCodeByte (LocalSet a)), fromIntegral a]
opCodeToByte (I32Const a) =
    B.pack [fromIntegral (opCodeByte (I32Const a)), fromIntegral a]
opCodeToByte (Call a) =
    B.pack [fromIntegral (opCodeByte (Call a)), fromIntegral a]
opCodeToByte (If a) =
    B.pack [fromIntegral (opCodeByte (If a)), fromIntegral (ifTypeByte a)]
opCodeToByte op = B.pack [fromIntegral (opCodeByte op)]

codeSectionCodeToByte :: CodeSectionCode -> B.ByteString
codeSectionCodeToByte (CSC a b lc ld e) =
    extendsBytes
        (B.pack (map fromIntegral (leb128Encode a)))
        [
            [B.pack [fromIntegral b]],
            map codeSectionCodeLocalsToByte lc,
            map opCodeToByte ld,
            [B.pack [fromIntegral e]]
        ]

codeSectionToByte :: CodeSection -> B.ByteString
codeSectionToByte (CS a b c ld) =
    extendsBytes
        (B.pack [fromIntegral a])
        [
            [B.pack (map fromIntegral (leb128Encode b))],
            [B.pack [fromIntegral c]],
            map (codeSectionCodeToByte) ld
        ]

wasmToByteString :: Wasm -> B.ByteString
wasmToByteString (Wasm hW vW tS fS mS eS cS) =
    extendBytes
        (headerWasmToByteString (Wasm hW vW tS fS mS eS cS))
        [
            typeSectionToByteString tS,
            functionSectionToByteString fS,
            memorySectionToByteString mS,
            exportSectionToByteString eS,
            codeSectionToByte cS
        ]

writeWasm :: Wasm -> FilePath -> IO ()
writeWasm w f = B.writeFile f bytes
    where
        bytes = wasmToByteString w
