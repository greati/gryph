module Execution.Memory where

import qualified Data.Map.Strict as M

import Syntactic.Values
import Syntactic.Syntax

{- Data memory implementation -}

-- Variable attributes
type Name   = String
type Scope  = String
type Scopes = [Scope]
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

updateVar :: Memory -> Name -> Scopes -> Cell -> Either String Memory
updateVar m n ss c@(t,v) = case fetchVar m n ss of
                            Left i -> Left i
                            Right ((_,s),(t',v')) -> if t /= t' then Left ("Incompatible typles " ++ show t ++ " and " ++ show t') 
                                                                else Right (M.update (\v -> Just c) (n,s) m)


fetchVar :: Memory -> Name -> Scopes -> Either String (CellIdentifier, Cell)
fetchVar m n [] = Left ("Variable " ++ n ++ " not found in any scope.")
fetchVar m n (s:ss) 
    | M.notMember (n,s) m   = fetchVar m n ss 
    | otherwise             = Right ((n,s), m M.! (n,s))

fetchVarValue :: Memory -> Name -> Scopes -> Either String Value
fetchVarValue m n ss = case fetchVar m n ss of
                            Left i -> Left i
                            Right (_,(t,v)) -> Right (head v)

getVarScopeValue :: Memory -> Name -> Scope -> Either String Value
getVarScopeValue m n s
    | M.notMember (n,s) m   = Left ("Variable " ++ n ++ " in scope " ++ s ++ " not declared.")
    | otherwise             = Right (head v)
        where (_,v) = (m M.!(n,s))
