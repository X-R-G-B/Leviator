
module Computing.Operators.EvaluateSymbol
  (
    evaluateSymbol
  ) where

import Types
import Computing.Defines

-- Evaluate a symbol and return its value
evaluateSymbol :: Env -> Symbol -> Maybe Tree
evaluateSymbol env smbl =
    case getSymbolValue env smbl of
        (_, Nothing) -> Nothing
        (_, Just (Number number)) -> Just (Number number)
        (_, Just (Boolean value)) -> Just (Boolean value)
        (_, Just (List list)) -> Just (List list)
        (_, _) -> Nothing
