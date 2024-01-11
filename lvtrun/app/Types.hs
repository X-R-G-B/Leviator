{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Types
-}

module Types
(
  TypeName(..),
  Limit(..),
  MemArg(..),
  Instruction(..),
  TypeIdx,
  FuncIdx,
  TableIdx,
  MemIdx,
  GlobalIdx,
  ElemIdx,
  DataIdx,
  LocalIdx,
  LabelIdx,
  FuncType(..),
  Import(..),
  ImportDesc(..),
  Function(..),
  Mutability(..),
  Global(..),
  ExportDesc(..),
  Export(..),
  Table(..),
  WasmModule(..),
  getTypeFromByte,
  ModHeader(..),
  FileContent,
  SectionID(..),
  Section(..),
  Memory(..),
  OpCode,
  Local(..)
) where

import Data.Int (Int32, Int64)
import Data.Word (Word8)
import Numeric (showHex)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import Errors

-- Indices
type TypeIdx = Int32
type FuncIdx = Int32
type TableIdx = Int32
type MemIdx = Int32
type GlobalIdx = Int32
type ElemIdx = Int32
type DataIdx = Int32
type LocalIdx = Int32
type LabelIdx = Int32

-- Common

type FileContent = BSL.ByteString

data TypeName =
  I32 |
  I64 |
  F32 |
  F64
  deriving (Show, Eq, Enum)

getTypeFromByte :: Word8 -> TypeName
getTypeFromByte 0x7f = I32
getTypeFromByte 0x7e = I64
getTypeFromByte 0x7d = F32
getTypeFromByte 0x7c = F64
getTypeFromByte _ = throw $ WasmError "GetTypeFromByte: bad type"

data Limit = Limit {
  lMin :: Int32,
  lMax :: Maybe Int32
}

instance Show Limit where
  show limit = "[\n\tmin: " ++ (show $ lMin limit) ++ "\n\tmax: " ++ (show $ lMax limit) ++ "\n]"

-----------------------

data MemArg = MemArg {
  offset :: Int32,
  align :: Int32
}

instance Show MemArg where
  show memArg = "[\n\toffset: " ++ (show $ offset memArg) ++ "\n\talign: " ++ (show $ align memArg) ++ "\n]"

type OpCode = [Word8]

data BlockType =
  EmptyType
  | ValType TypeName
  | TypeIdx TypeIdx
  deriving (Show)

data Instruction =
  Unreachable
  | Nop
  | Return
  | Call FuncIdx
  | I32Const Int32
  | I64Const Int64
  | F32Const Float
  | F64Const Double
  | I32Load MemArg
  | I64Load MemArg
  | I32Store MemArg
  | I64Store MemArg
  | GetLocal LocalIdx
  | SetLocal LocalIdx
  | GetGlobal GlobalIdx
  | SetGlobal GlobalIdx
  | I32Add
  | I32Sub
  | I32And
  | I32Eqz
  | I32Gtu
  | I32Leu
  | LocalTee LocalIdx
  | BrIf LabelIdx
  | Br LabelIdx
  | Block BlockType
  | End
  | MemorySize
  | MemoryGrow

instance Show Instruction where
  show Unreachable = "\n\t\t\t\tunreachable"
  show Nop = "\n\t\t\t\tnop"
  show Return = "\n\t\t\t\treturn"
  show (Call idx) = "\n\t\t\t\tcall " ++ (show idx)
  show (I32Const value) = "\n\t\t\t\ti32.const " ++ (show value)
  show (I64Const value) = "\n\t\t\t\ti64.const " ++ (show value)
  show (F32Const value) = "\n\t\t\t\tf32.const " ++ (show value)
  show (F64Const value) = "\n\t\t\t\tf64.const " ++ (show value)
  show (I32Load memArg) = "\n\t\t\t\ti32.load " ++ (show memArg)
  show (I64Load memArg) = "\n\t\t\t\ti64.load " ++ (show memArg)
  show (I32Store memArg) = "\n\t\t\t\ti32.store " ++ (show memArg)
  show (I64Store memArg) = "\n\t\t\t\ti64.store " ++ (show memArg)
  show (GetLocal idx) = "\n\t\t\t\tget_local " ++ (show idx)
  show (SetLocal idx) = "\n\t\t\t\tset_local " ++ (show idx)
  show (GetGlobal idx) = "\n\t\t\t\tget_global " ++ (show idx)
  show (SetGlobal idx) = "\n\t\t\t\tset_global " ++ (show idx)
  show I32Add = "\n\t\t\t\ti32.add"
  show I32Sub = "\n\t\t\t\ti32.sub"
  show MemorySize = "\n\t\t\t\tmemory.size"
  show MemoryGrow = "\n\t\t\t\tmemory.grow"
  show I32And = "\n\t\t\t\ti32.and"
  show I32Eqz = "\n\t\t\t\ti32.eqz"
  show I32Gtu = "\n\t\t\t\ti32.gt_u"
  show I32Leu = "\n\t\t\t\ti32.le_u"
  show (LocalTee idx) = "\n\t\t\t\tlocal.tee " ++ (show idx)
  show (BrIf idx) = "\n\t\t\t\tbr_if " ++ (show idx)
  show (Br idx) = "\n\t\t\t\tbr " ++ (show idx)
  show End = "\n\t\t\t\tend"
  show (Block blockType) = "\n\t\t\t\tblock " ++ (show blockType)

-- Module section

data Local = Local {
  lcIdx :: LocalIdx,
  lcType :: TypeName
}

instance Show Local where
  show local = "\n\t\t(local idx:" ++ (show $ lcIdx local) ++ " type:" ++ (show $ lcType local) ++ ")"

data FuncType = FuncType {
  typeId :: TypeIdx,
  params :: [TypeName],
  results :: [TypeName]
}

instance Show FuncType where
  show funcType = "\n\t(type " ++ (show $ typeId funcType) ++ " (func " ++
    (show $ params funcType) ++ ") " ++ (show $ results funcType) ++ ")"

data Import = Import {
  mod :: String,
  name :: String,
  desc :: ImportDesc
} deriving (Show)

data ImportDesc =
  ImportFunc TypeIdx |
  ImportMemory Limit
  deriving (Show)

data Function = Function {
  funcType :: TypeIdx,
  funcIdx :: FuncIdx,
  locals :: [Local],
  body :: [Instruction]
}

instance Show Function where
  show func = "\n\t(func idx:" ++ (show $ funcIdx func) ++ " typeId:" ++ (show $ funcType func) ++ " " ++ (show $ locals func) ++ "\nIntructions:\n" ++ (show $ body func) ++ ")"

type Memory = Limit

data Mutability = Const | Var deriving (Show)

data Global = Global {
  globalType :: TypeName,
  mutability :: Mutability,
  initExpr :: [Instruction]
}

instance Show Global where
  show global = "\n\t(global " ++ (show $ globalType global) ++ " " ++
    (show $ mutability global) ++ " " ++ (show $ initExpr global) ++ ")"


data ExportDesc =
  ExportFunc FuncIdx
  | ExportTable TableIdx
  | ExportMemory MemIdx
  | ExportGlobal GlobalIdx
  deriving (Show)

data Export = Export {
  expName :: String,
  expDesc :: ExportDesc
}

instance Show Export where
  show export = "\n\t(export \"" ++ (expName export) ++ "\" " ++ (show $ expDesc export) ++ ")"

data Table = Table {
  notImpl :: String
} deriving (Show)

data ModHeader = ModHeader {
  magicNumber :: BSL.ByteString,
  version :: BSL.ByteString
} deriving (Show)

data SectionID =
  CustomID
  | TypeID
  | ImportID
  | FunctionID
  | TableID
  | MemoryID
  | GlobalID
  | ExportID
  | StartID
  | ElementID
  | CodeID
  | DataID
  | DataCountID
  | InvalidID
  deriving (Show, Eq)

data Section = Section {
  identifier :: SectionID,
  size :: Int,
  content :: BSL.ByteString
}

instance Show Section where
  show section =
    "\nSection " ++ (show $ identifier section) ++
    " Size: " ++ (show $ size section) ++
    " Content: " ++ (concat $ map (\x -> showHex x " ") (BSL.unpack $ content section))

data WasmModule = WasmModule {
  header :: ModHeader,
  types :: [FuncType],
  imports :: [Import],
  functions :: [Function],
  tables :: [Table],
  memory :: Memory,
  globals :: [Global],
  exports :: [Export]
}

instance Show WasmModule where
  show wasmMod = "\n[ Wasm Module Header ]\n" ++
    "- Header: " ++ (show $ header wasmMod) ++ "\n" ++
    "- Types: " ++ (show $ types wasmMod) ++ "\n" ++
    "- Imports: " ++ (show $ imports wasmMod) ++ "\n" ++
    "- Functions: " ++ (show $ functions wasmMod) ++ "\n" ++
    "- Tables: " ++ (show $ tables wasmMod) ++ "\n" ++
    "- Memory: " ++ (show $ memory wasmMod) ++ "\n" ++
    "- Globals: " ++ (show $ globals wasmMod) ++ "\n" ++
    "- Exports: " ++ (show $ exports wasmMod) ++ "\n"
