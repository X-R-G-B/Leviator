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
  Memory,
  Local(..),
  BlockType(..),
  Value(..)
) where

import Data.Int (Int32, Int64)
import Data.Word (Word8)
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

type FileContent = BSL.ByteString

type Memory = Limit

data TypeName =
  I32
  | I64
  | F32
  | F64
  deriving (Show, Eq, Enum)

data Limit = Limit {
  lMin :: Int32,
  lMax :: Maybe Int32
} deriving (Show, Eq)

data MemArg = MemArg {
  offset :: Int32,
  align :: Int32
} deriving (Show)

instance Eq MemArg where
  (==) memArg1 memArg2 = (offset memArg1) == (offset memArg2)
    && (align memArg1) == (align memArg2)

data BlockType =
  EmptyType
  | ValType TypeName
  | TypeIdx TypeIdx
  deriving (Show, Eq)

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
  | I32Mul
  | I32Divs
  | I32Eqz
  | I32Gtu
  | I32Leu
  | I32Eq
  | I32Lts
  | Else
  | I32Gts
  | I32Les
  | I32Ges
  | I32Ne
  | Loop
  | LocalTee LocalIdx
  | BrIf LabelIdx
  | If
  | Br LabelIdx
  | Block BlockType
  | End
  | MemorySize
  | MemoryGrow
  deriving (Eq)
--IF/ELSE
--LOOP
--BR

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
  show I32Mul = "\n\t\t\t\ti32.mul"
  show I32Divs = "\n\t\t\t\ti32.div_s"
  show MemorySize = "\n\t\t\t\tmemory.size"
  show MemoryGrow = "\n\t\t\t\tmemory.grow"
  show I32And = "\n\t\t\t\ti32.and"
  show I32Eqz = "\n\t\t\t\ti32.eqz"
  show I32Gts = "\n\t\t\t\ti32.gt_s"
  show I32Les = "\n\t\t\t\ti32.le_s"
  show I32Ne = "\n\t\t\t\ti32.ne"
  show I32Ges = "\n\t\t\t\ti32.ge_s"
  show I32Lts = "\n\t\t\t\ti32.lt_s"
  show I32Gtu = "\n\t\t\t\ti32.gt_u"
  show I32Leu = "\n\t\t\t\ti32.le_u"
  show If = "\n\t\t\t\tif"
  show I32Eq = "\n\t\t\t\ti32.eq"
  show (LocalTee idx) = "\n\t\t\t\tlocal.tee " ++ (show idx)
  show (BrIf idx) = "\n\t\t\t\tbr_if " ++ (show idx)
  show (Br idx) = "\n\t\t\t\tbr " ++ (show idx)
  show End = "\n\t\t\t\tend"
  show (Block blockType) = "\n\t\t\t\tblock " ++ (show blockType)
  show (Loop) = "\n\t\t\t\tloop"

-- Module section

data Value =
  I_32 Int32
  | I_64 Int64
  | F_32 Float
  | F_64 Double
  deriving (Eq)

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

instance Show Value where
  show (I_32 val) = show val
  show (I_64 val) = show val
  show (F_32 val) = show val
  show (F_64 val) = show val

data Local = Local {
  lcIdx :: LocalIdx,
  lcType :: TypeName
} deriving (Show)

data FuncType = FuncType {
  typeId :: TypeIdx,
  params :: [TypeName],
  results :: [TypeName]
} deriving (Show)

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
} deriving (Show)

data Mutability = Const | Var deriving (Show)

data Global = Global {
  globalType :: TypeName,
  mutability :: Mutability,
  initExpr :: [Instruction]
} deriving (Show)

data ExportDesc =
  ExportFunc FuncIdx
  | ExportTable TableIdx
  | ExportMemory MemIdx
  | ExportGlobal GlobalIdx
  deriving (Show)

data Export = Export {
  expName :: String,
  expDesc :: ExportDesc
} deriving (Show)

data Table = Table {
  notImpl :: String
} deriving (Show)

data ModHeader = ModHeader {
  magicNumber :: BSL.ByteString,
  version :: BSL.ByteString
} deriving (Show)

data Section = Section {
  identifier :: SectionID,
  size :: Int,
  content :: BSL.ByteString
} deriving (Show)

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

getTypeFromByte :: Word8 -> TypeName
getTypeFromByte 0x7f = I32
getTypeFromByte 0x7e = I64
getTypeFromByte 0x7d = F32
getTypeFromByte 0x7c = F64
getTypeFromByte _ = throw $ WasmError "GetTypeFromByte: bad type"
