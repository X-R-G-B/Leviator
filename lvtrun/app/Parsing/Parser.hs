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
import Parsing.Sections
import Parsing.Header
import Parsing.Memory
import Parsing.FuncTypes
import Parsing.Global
import Parsing.Exports
import Parsing.Functions
import Parsing.Code

import Debug.Trace

parseModule :: FileContent -> WasmModule
parseModule bytes = do
  let sections = getSections bytes
  WasmModule (getModHeader (getSectionWithId sections CustomID))
    (getFuncTypes (getSectionWithId sections TypeID))
      []
        ((getFuncCode (getSectionWithId sections CodeID) (getFunctions (getSectionWithId sections FunctionID))))
          []
            (getMemories (getSectionWithId sections MemoryID))
              (getGlobals (getSectionWithId sections GlobalID))
                (getExports (getSectionWithId sections ExportID))
