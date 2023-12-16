
module ListContainList 
    (
        doesListContainsList
    ) where

import Types

doesListContainsList :: [Tree] -> Bool
doesListContainsList [] = False
doesListContainsList (List _ : _) = True
doesListContainsList (_ : rest) = doesListContainsList rest
