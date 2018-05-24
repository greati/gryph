module Execution.Memory where

import qualified Data.Map.Strict as M

import Syntactic.Values
import Syntactic.Syntax

{- Data memory implementation -}

-- Variable attributes
type Name   = String
type Scope  = String
type Values = [Value]

-- Memory structure
type CellIdentifier = (Name, Scope)
type Cell           = (GType, Values)

type Memory         = M.Map CellIdentifier Cell

memory = M.empty

elabVar :: Scope -> Name -> Cell -> Memory -> Either String Memory
elabVar s n c m 
    | M.member ci m = Left ("Variable " ++ n ++ " in scope " ++ s ++ " already declared.")
    | otherwise     = Right (M.insert ci c m)
        where ci = (n,s)

fetchVarValue :: Memory -> Name -> Scope -> Either String Value
fetchVarValue m n s
    | M.notMember (n,s) m   = Left ("Variable " ++ n ++ " in scope " ++ s ++ " not declared.")
    | otherwise             = Right (head v)
        where (_,v) = (m M.!(n,s))
