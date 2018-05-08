module Execution.Memory where

import qualified Data.Map as M

import Syntactic.Values

{- Language memory implementation -}

-- Variable attributes
type Name   = String
type Scope  = String
type Values = [Value]

-- Memory structure
type CellIdentifier = (Name, Scope)
type Cell           = Values

type Memory         = M.Map CellIdentifier Cell

