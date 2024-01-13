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
import qualified Data.ByteString as B
import WatAST (OpCode (..))
import Data.Char (ord)

-- extend

extendBytes :: B.ByteString -> [B.ByteString] -> B.ByteString
extendBytes l [] = l
extendBytes l (x:xs) = extendBytes (l `B.append` x) xs

extendsBytes :: B.ByteString -> [[B.ByteString]] -> B.ByteString
extendsBytes a [] = a
extendsBytes a (x:xs) = extendsBytes (extendBytes a x) xs

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
    extendBytes
        (B.pack (map fromIntegral [a, b, c]))
        (map typeSectionTypeToByteString ld)

--

functionSectionToByteString :: FunctionSection -> B.ByteString
functionSectionToByteString (FS a b c ld) =
    B.pack (map fromIntegral ([a, b, c] ++ ld))

--

memorySectionToByteString :: MemorySection -> B.ByteString
memorySectionToByteString (MS a b 0 d _) =
    B.pack (map fromIntegral ([a, b, 0, d]))
memorySectionToByteString (MS a b _ d e) =
    B.pack (map fromIntegral ([a, b, 1, d, e]))

--

exportSectionExportToByteString :: ExportSectionExport -> B.ByteString
exportSectionExportToByteString (ESE a lb c d) =
    B.pack (map fromIntegral ([a] ++ (map ord lb) ++ [exportSectionExportTypeByte c, d]))

exportSectionToByteString :: ExportSection -> B.ByteString
exportSectionToByteString (ES a b c ld) =
    extendBytes
        (B.pack (map fromIntegral [a, b, c]))
        (map exportSectionExportToByteString ld)

--

codeSectionCodeLocalsToByte :: CodeSectionCodeLocals -> B.ByteString
codeSectionCodeLocalsToByte (a, b) =
    B.pack (map fromIntegral [fromIntegral a, variableTypeByte b])

opCodeToByte :: OpCode -> B.ByteString
opCodeToByte (LocalGet a) = B.pack (map fromIntegral [opCodeByte (LocalGet a), fromIntegral a])
opCodeToByte (LocalSet a) = B.pack (map fromIntegral [opCodeByte (LocalSet a), fromIntegral a])
opCodeToByte (I32Const a) = B.pack (map fromIntegral [opCodeByte (I32Const a), fromIntegral a])
opCodeToByte op = B.pack [fromIntegral (opCodeByte op)]

codeSectionCodeToByte :: CodeSectionCode -> B.ByteString
codeSectionCodeToByte (CSC a b lc ld e) =
    extendsBytes
        (B.pack (map fromIntegral [a, b]))
        [
            (map codeSectionCodeLocalsToByte lc),
            (map opCodeToByte ld),
            ([B.pack [fromIntegral e]])
        ]

codeSectionToByte :: CodeSection -> B.ByteString
codeSectionToByte (CS a b c ld) =
    extendBytes
        (B.pack (map fromIntegral [a, b, c]))
        (map codeSectionCodeToByte ld)

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
