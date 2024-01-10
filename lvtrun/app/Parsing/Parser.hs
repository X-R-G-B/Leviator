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

import Debug.Trace

parseModule :: FileContent -> WasmModule
parseModule bytes = do
  let sections = getSections bytes
  WasmModule (getModHeader (getSectionWithId sections CustomID))
    (getFuncTypes (getSectionWithId sections TypeID))
      []
        []
          []
            (getMemories (getSectionWithId sections MemoryID))
              (getGlobals (getSectionWithId sections GlobalID))
                []
