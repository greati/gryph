module Execution.Memory where

import qualified Data.Map.Strict as M

import Syntactic.Values
import Syntactic.Syntax

{- Program memory implementation.
 -
 -}
type SubIdentifier = (String, [GParamType]) 
type SubContent = ([(String, GParamType)], Maybe GType, Block)
type ProgramMemory = M.Map SubIdentifier SubContent

declareSubprogram :: SubIdentifier -> SubContent -> ProgramMemory -> Either String ProgramMemory 
declareSubprogram id@(n,ts) content m
    | M.member id m   = Left $ "Two equal declarations for subprogram " ++ n
    | otherwise       = Right (M.insert id content m)

fetchForSubprogramCall :: String -> [GType] -> ProgramMemory -> Either String Block
fetchForSubprogramCall s ts pm = undefined

chooseSuitableSubprogram :: [GType] -> [SubIdentifier] -> Either String SubIdentifier
chooseSuitableSubprogram ts is = undefined


{- Data memory implementation -}

-- Variable attributes
type Name   = String
type Scope  = String
type Scopes = [Scope]
type Values = [MemoryValue]

data MemoryValue = Value Value | Ref CellIdentifier deriving (Show, Eq)


-- Memory structure
type CellIdentifier = (Name, Scope)
type Cell           = (GType, MemoryValue)

type Memory         = M.Map CellIdentifier Cell

memory = M.empty

makeMemoryValue :: Either CellIdentifier Value -> MemoryValue
makeMemoryValue v = case v of
                        Left c -> Ref c
                        Right v -> Value v

elabVar :: Scope -> Name -> Cell -> Memory -> Either String Memory
elabVar s n c@(t,v) m 
    | M.member ci m = Left ("Variable " ++ n ++ " in scope " ++ s ++ " already declared.")
    | otherwise     = Right (M.insert ci (t,v) m) 
        where ci = (n,s)

updateVar :: Memory -> Name -> Scopes -> Cell -> Either String Memory
updateVar m n ss c@(t,v) = case fetchVar m n ss of
                            Left i -> Left i
                            Right ((_,s),(t',v')) -> Right (M.update (\k -> Just (t, v)) (n,s) m)


fetchVar :: Memory -> Name -> Scopes -> Either String (CellIdentifier, Cell)
fetchVar m n [] = Left ("Variable " ++ n ++ " not found in any visible scope.")
fetchVar m n (s:ss) 
    | M.notMember (n,s) m   = fetchVar m n ss 
    | otherwise             = Right ((n,s), m M.! (n,s))

fetchVarValue :: Memory -> Name -> Scopes -> Either String Value
fetchVarValue m n ss = case fetchVar m n ss of
                            Left i -> Left i
                            Right (_,(t,v)) -> case v of 
                                                    Value v' -> Right v'
                                                    Ref (n',s') -> getVarScopeValue m n' s'
                                                    
getVarScopeValue :: Memory -> Name -> Scope -> Either String Value
getVarScopeValue m n s
    | M.notMember (n,s) m   = Left ("Variable " ++ n ++ " in scope " ++ s ++ " not declared.")
    | otherwise             = v
        where v = case (m M.!(n,s)) of
                    (_,Value v) -> Right v
                    (_,Ref (n',s')) -> getVarScopeValue m n' s'

clearScope :: Scope -> Memory -> Memory
clearScope s m = M.filterWithKey (\(_,s') _ -> s' /= s) m 




