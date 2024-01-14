{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Parser
-}

module Parsing.Parser
(
  parseModule,
)
where

import Types
import qualified Parsing.Header as PH
import qualified Parsing.FuncTypes as FT
import qualified Parsing.Functions as FN
import qualified Parsing.Memory as M
import qualified Parsing.Exports as E
import qualified Parsing.Sections as S
import qualified Parsing.Global as G
import qualified Parsing.Code as C

parseModule :: FileContent -> WasmModule
parseModule bytes =
  WasmModule {
    header = PH.getModHeader (S.getSectionWithId sections CustomID),
    types = FT.getFuncTypes (S.getSectionWithId sections TypeID),
    imports = [],
    functions = C.getFuncCode codeSection funcs,
    tables = [],
    globals = G.getGlobals (S.getSectionWithId sections GlobalID),
    memory = M.getMemories (S.getSectionWithId sections MemoryID),
    exports = E.getExports (S.getSectionWithId sections ExportID)
  }
  where
    sections = S.getSections bytes
    codeSection = S.getSectionWithId sections CodeID
    funcs = FN.getFunctions (S.getSectionWithId sections FunctionID)
