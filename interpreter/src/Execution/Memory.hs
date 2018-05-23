module Execution.Memory where

import qualified Data.Map.Strict as M

import Syntactic.Values
import Syntactic.Syntax

{- Language memory implementation -}

-- Variable attributes
type Name   = String
type Scope  = String
type Values = [Value]

-- Memory structure
type CellIdentifier = (Name, Scope)
type Cell           = ( GType, Values)

type Memory         = M.Map CellIdentifier Cell

memory = M.empty

elab:: CellIdentifier -> Cell -> Memory -> Memory
elab  ci c m  = M.insert ci c m 

elabList :: VarDeclaration -> Memory -> Memory
elabList = undefined 
--elabList (VarDeclaration is t es ) 
