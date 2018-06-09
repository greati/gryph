{-# LANGUAGE BangPatterns #-}
module Execution.Memory where

import qualified Data.Map.Strict as M

import Syntactic.Values
import Syntactic.Syntax
import Syntactic.Types

{- Program memory implementation.
 -
 - Memory to store subprograms and structs.
 -}

type StructIdentifier = String 
type StructField = (Name, GType, Maybe Value)
type StructContent = [StructField]

type SubIdentifier = (String, [GParamType]) 
type FormalParameter = (String, GParamType, Maybe Value)
type SubContent = ([FormalParameter], Maybe GType, Block)

data UnitIdentifier = SubIdentifier SubIdentifier | StructIdentifier StructIdentifier deriving (Eq, Show, Ord)
data UnitContent = SubContent SubContent | StructContent StructContent deriving (Eq, Show)

-- | The program memory.
-- Used to store subprogram declarations and structures.
type ProgramMemory = M.Map UnitIdentifier UnitContent

type ProcessedActualParams = [(Either (Identifier, Either CellIdentifier (Maybe Value)) Value, GType)]

programMemory = M.empty

declareSubprogram :: SubIdentifier -> SubContent -> ProgramMemory -> Either String ProgramMemory
declareSubprogram id@(n,ts) content m
    | M.member (SubIdentifier id) m   = Left $ "Two equal declarations for subprogram " ++ n
    | otherwise                       = Right (M.insert (SubIdentifier id) (SubContent content) m)

fetchSubprograms :: Name -> ProcessedActualParams -> ProgramMemory -> [(SubIdentifier, SubContent)]
fetchSubprograms n ts pm = map obtainSubprogram (M.toAscList (M.filterWithKey test pm))
    where 
        test (SubIdentifier (n',_)) (SubContent (ps,_,_)) = n'==n && length ts >= countNecessaryParams ps && length ts <= length ps
        test _ _ = False

        obtainSubprogram :: (UnitIdentifier, UnitContent) -> (SubIdentifier, SubContent)
        obtainSubprogram (SubIdentifier si, SubContent sc) = (si, sc)
        obtainSubprogram _ = error "Not a valid subprogram"

selectSubForCall :: Name -> ProcessedActualParams -> ProgramMemory -> Maybe (SubIdentifier, SubContent)
selectSubForCall n ts pm = case v of 
                                Just _ -> Just (possibilities !! i)
                                Nothing -> Nothing
     where        
            (v,i) = foldl maxMaybe (Nothing, (-1)) enumerated
            maxMaybe (v,i) x''@(Nothing ,i') = (v,i)
            maxMaybe (v,i) x''@(x'@(Just s'),i') = case v of
                                                        Nothing -> x''
                                                        Just s -> if s > s' then (v,i) 
                                                                  else 
                                                                    if s == s' then error "Ambiguous call"
                                                                    else x''
            enumerated = zip scores [0..(length scores - 1)]
            scores = map (scoreSubForCall ts 0 []) possibilities
            possibilities = fetchSubprograms n ts pm

scoreSubForCall :: ProcessedActualParams -> Int -> [Name] -> (SubIdentifier, SubContent) -> Maybe Int
scoreSubForCall [] _ _ _ = Just 0
scoreSubForCall params@((p,t):ps) pos nameds contents@(si,(fs, _, _)) = case p of
                                        Left ((Ident i), Right (Just v)) -> case scoreSubForCall ps pos (i:nameds) contents of
                                                Just score -> if f' /= [] then 
                                                                if (getParamGType t') == t then Just (1 + score) 
                                                                    else 
                                                                        if compatType (getParamGType t') t then
                                                                            Just (score) 
                                                                        else Nothing
                                                              else Nothing
                                                    where 
                                                        (_,t',_) = head f' 
                                                        f' = filter (\(i',t', _)-> i == i') fs
                                                Nothing -> Nothing
                                        _ -> if nameds /= [] then Nothing
                                                else case scoreSubForCall ps (pos + 1) nameds contents of
                                                    Just score -> if getParamGType t' == t then
                                                                        Just (1 + score)
                                                                  else
                                                                        if compatType (getParamGType t') t then
                                                                            Just score
                                                                        else Nothing
                                                    Nothing -> Nothing
                                                    where 
                                                        (_,t',_) = fs !! pos
compatType :: GType -> GType -> Bool
compatType t t' = if t == t' then True
                    else case (t,t') of
                        (GFloat, GInteger) -> True
                        (GUserType _, GAnonymousStruct) -> True
                        _ -> False

getParamGType :: GParamType -> GType  
getParamGType (GRef t) = t
getParamGType (GType t) = t

countNecessaryParams :: [(String, GParamType, Maybe Value)] -> Int
countNecessaryParams ps = foldr (\(_,_,v) s -> if v == Nothing then s+1 else s) 0 ps

-- | Insert a user type in program memory
declareStruct :: ProgramMemory -> StructIdentifier -> StructContent -> Either String ProgramMemory
declareStruct m si sc 
    | M.member (StructIdentifier si) m = Left $ "User type " ++ si ++ " already declared"
    | otherwise                        = Right (M.insert (StructIdentifier si) (StructContent sc) m)

-- | Fetch a user type declaration from program memory
fetchStructDecl :: ProgramMemory -> Name -> (StructIdentifier, StructContent)
fetchStructDecl m n 
    | M.notMember (StructIdentifier n) m = error $ "User type " ++ n ++ " not declared"
    | otherwise = (n, v)
        where (StructContent v) = m M.! (StructIdentifier n)

{- Data memory implementation -}

-- Variable attributes
type Name   = String
data Scope = GlobalScope | SubScope Integer | IterationScope Integer | BlockScope Integer deriving (Eq, Show, Ord)
--type Scope  = String
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
    | M.member ci m = Left $ "Redeclaration of variable " ++ n ++ "in scope " ++ show s
    | otherwise     = case t of 
                        --GUserType t' -> undefined 
                        _ -> Right (M.insert ci (t,v) m) 
        where ci = (n,s)

elabVars :: Memory -> [(Name,Cell)] -> Scope -> Either String Memory
elabVars m [] s = Right m
elabVars m (v@(n,c):vs) s = case elabVar s n c m of
                                Left i -> Left i
                                Right m' -> elabVars m' vs s

updateVarScope :: Memory -> Name -> Scope -> Cell -> Either String Memory
updateVarScope m n s c 
    | M.notMember (n,s) m = Left ("Variable " ++ n ++ " not found for the given scope")
    | otherwise = Right $ (M.update (\k -> Just c) (n,s) m)

updateVar :: Memory -> Name -> Scopes -> Cell -> Either String Memory
updateVar m n ss c@(t,v) = case fetchVar m n ss of
                            Left i -> Left i
                            Right ((_,s),(t',v')) -> case v' of
                                                        Ref (n',s') -> updateVarScope m n' s' c
                                                        _ -> Right (M.update (\k -> Just (t, v)) (n,s) m)

fetchVarCell :: Memory -> Name -> Scopes -> Either String Cell
fetchVarCell m n ss = case fetchVar m n ss of
                        Left i -> Left i
                        Right (ci,c) -> Right c

fetchVar :: Memory -> Name -> Scopes -> Either String (CellIdentifier, Cell)
fetchVar m n [] = Left ("Variable " ++ n ++ " not found in any visible scope.")
fetchVar m n (s:ss) 
    | M.notMember (n,s) m   = fetchVar m n ss 
    | otherwise             = Right ((n,s), m M.! (n,s))

fetchVarValueType :: Memory -> Name -> Scopes -> Either String (GType,Value)
fetchVarValueType m n ss = case fetchVar m n ss of
                            Left i -> Left i
                            Right (_,(t,v)) -> case v of 
                                                    Value v' -> Right (t,v')
                                                    --r@(Register _) -> Right $ (t, makeSetterFromRegister r)
                                                    Ref (n',s') -> Right $ (t,v'')
                                                        where v'' = case getVarScopeValue m n' s' of
                                                                        Left i -> error i
                                                                        Right i -> i

fetchVarValue :: Memory -> Name -> Scopes -> Either String Value
fetchVarValue m n ss = case fetchVar m n ss of
                            Left i -> Left i
                            Right (_,(t,v)) -> case v of 
                                                    Value v' -> Right v'
                                                    --r@(Register _) -> Right $ makeSetterFromRegister r
                                                    Ref (n',s') -> getVarScopeValue m n' s'
                                                    
fetchCellByScope :: Memory -> Name -> Scope -> Either String Cell
fetchCellByScope m n s
    | M.notMember (n,s) m   = Left ("Variable " ++ n ++ " not found for the given scope")
    | otherwise             = Right v
        where v = (m M.!(n,s))
                    --case (m M.!(n,s)) of
                    --(_,Value v) -> Right v
                    --(_,Ref (n',s')) -> getVarScopeValue m n' s'

getVarScopeValue :: Memory -> Name -> Scope -> Either String Value
getVarScopeValue m n s
    | M.notMember (n,s) m   = Left ("Variable " ++ n ++ " not found for the given scope")
    | otherwise             = v
        where v = case (m M.!(n,s)) of
                    (_,Ref (n',s')) -> getVarScopeValue m n' s'
                    (_,Value v) -> Right v
                    --(_,r@(Register v)) -> Right $ makeSetterFromRegister r

-- | Default values for each type
defaultValue :: ProgramMemory -> GType -> Value
defaultValue pm GInteger = Integer 0
defaultValue pm GFloat = Float 0.0
defaultValue pm GString = String []
defaultValue pm GBool = Bool False
defaultValue pm GChar = Char '\0'
defaultValue pm (GList _) = List []
defaultValue pm (GPair t1 t2) = Pair (defaultValue pm t1, defaultValue pm t2)
defaultValue pm (GTriple t1 t2 t3) = Triple (defaultValue pm t1, defaultValue pm t2, defaultValue pm t3)
defaultValue pm (GQuadruple t1 t2 t3 t4) = Quadruple (defaultValue pm t1, defaultValue pm t2, defaultValue pm t3, defaultValue pm t4)
defaultValue pm (GDict k v) = Map (M.empty)
defaultValue pm (GUserType u) = makeSetterFromDeclaration pm (u,sc)
    where (si, sc) = fetchStructDecl pm u

-- | Register to setter
--makeSetterFromRegister :: MemoryValue -> Value
--makeSetterFromRegister (Register mr) = Setter $ M.fromList (map (\(n,(t,Value v))->(n,v)) (M.toList mr))

--Register (M.Map Name Cell)
--M.Map String Value

-- | Declaration to Setter
makeSetterFromDeclaration :: ProgramMemory -> (StructIdentifier, StructContent) -> Value
makeSetterFromDeclaration pm (si,scs'@((n,t,mv):scs)) = Setter si (makeMap scs')
        where 
                makeMap :: StructContent -> M.Map String (GType,Value)
                makeMap [] = M.empty
                makeMap scs'@((n,t,mv):scs) = M.insert n (t,v') m'
                    where   m' = makeMap scs
                            v' = case mv of
                                    Nothing -> defaultValue pm t
                                    Just v'' -> v'' 

clearScope :: Scope -> Memory -> Memory
clearScope s m = M.filterWithKey (\(_,s') _ -> s' /= s) m 




