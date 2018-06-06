module Execution.Semantic where

import Syntactic.Values
import Syntactic.Syntax
import Execution.Memory
import Data.Time.Clock
import qualified Data.Map as M

type Filename = String

-- | Get current system time to produce scope ids
getCurSeconds :: IO Integer                                                                                   
getCurSeconds = do                                                                                            
                    t <- getCurrentTime                                                                       
                    i <- return $ utctDayTime t                                                               
                    return $ diffTimeToPicoseconds i


-- |Scopes in the execution.
scopes :: Scopes
scopes = [GlobalScope]

-- |Execute a program represented as a list of program units.
exec :: Memory -> ProgramMemory -> Scopes -> [ProgramUnit] -> IO() 
exec m pm ss [] = return ()
exec m pm ss (u:us) = do
                        (m', pm', ss') <- execUnit u m pm ss
                        print $ show m' 
                        exec m' pm' ss' us 

-- |Executes a program unit.
execUnit :: ProgramUnit -> Memory -> ProgramMemory -> Scopes -> IO (Memory, ProgramMemory, Scopes)
execUnit (SubprogramDecl sub) m pm ss = do 
                                            pm' <- execSubDecl sub m pm ss
                                            return (m, pm', ss)
execUnit (StructDecl struct) m pm ss = 
                                do
                                    pm' <- execStructDecl m pm ss struct
                                    return (m, pm', ss)

execUnit (Stmt stmt) m pm ss = do 
                                    (m', ss', v) <- execStmt stmt m pm ss
                                    return (m', pm, ss')

-- |Executes a subprogram declaration.
execSubDecl :: Subprogram -> Memory -> ProgramMemory -> Scopes -> IO (ProgramMemory)
execSubDecl s m pm ss = do  
                            (si,sc) <- interpretSubDeclaration s m pm ss
                            case declareSubprogram si sc pm of
                                Left i -> error i
                                Right pm' -> return pm'

-- |Executes a struct declaration.
execStructDecl :: Memory -> ProgramMemory -> Scopes -> StructDecl -> IO (ProgramMemory)
execStructDecl m pm ss s = do
                                (si, sc) <- interpretStructDecl m pm ss s
                                case declareStruct pm si sc of
                                    Left i -> error i
                                    Right pm' -> return pm'

type Scoper = (Integer -> Scope)

-- |Executes a block of statement, creating a new scope. When finish, clear the scope.
-- It also allows taking a list of declarations in the form [(Name,Cell)]
execBlock :: Block -> Memory -> ProgramMemory -> Scoper -> Scopes -> [(Name,Cell)] -> IO (Memory, Scopes, Maybe Value)
execBlock (Block []) m pm _ ss _ = return (m, ss, Nothing)
execBlock b@(Block (st:sts)) m pm scoper ss decls =  do time <- getCurSeconds
                                                        let newScope = scoper (time) in
                                                            let ss' = (newScope:ss) in
                                                                let m' = case elabVars m decls newScope of
                                                                        Left i -> error i
                                                                        Right m'' -> m'' in
                                                                            do 
                                                                                execBlock' b m' pm ss' newScope
                                                                            where
                                                                                    execBlock' (Block []) m pm (s:ss) newScope = return (clearScope s m, ss, Nothing)
                                                                                    execBlock' (Block (st:sts)) m pm ss' newScope = do 
                                                                                                    (m'',ss'', v'') <- execStmt st m pm ss'
                                                                                                    do
                                                                                                        if newScope == head ss'' then
                                                                                                            execBlock' (Block sts) m'' pm ss'' newScope
                                                                                                        else 
                                                                                                            return $ (m'',ss'',v'')

-- |Executes any statement.
execStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO (Memory, Scopes, Maybe Value)
execStmt d@(DeclStmt _) m pm ss = do
                            m' <- varDeclStmt d m pm ss
                            return (m', ss, Nothing)
execStmt a@(AttrStmt _ _) m pm ss = do
                            m' <- execAttrStmt a m pm ss
                            return (m', ss, Nothing)
execStmt (PrintStmt e) m pm ss = do
                                v <- eval m pm ss e
                                putStrLn (show v)
                                return (m, ss, Nothing)
execStmt (ReadStmt i) m pm ss = do
                                value <- getLine
                                case updateVar m ((\(Ident i) -> i) i) ss (makeCompatibleAssignTypes pm GString (String value)) of
                                    Left e -> error e
                                    Right m' -> return (m', ss, Nothing)
                                

execStmt (IfStmt e (IfBody ifbody) elsebody) m pm ss' = do 
                            v <- eval m pm ss' e
                            let test = case makeBooleanFromValue v of
                                            Left i -> error i
                                            Right i -> i in 
                                do 
                                    if test then
                                        case ifbody of
                                            (CondStmt st) -> do execBlock (Block [st]) m pm BlockScope ss' []
                                            (CondBlock block) -> do execBlock block m pm BlockScope ss' []
                                    else
                                        case elsebody of
                                            NoElse -> do return (m,ss', Nothing)
                                            ElseBody (CondStmt st) -> do execBlock (Block [st]) m pm BlockScope ss' []
                                            ElseBody (CondBlock block) -> do execBlock block m pm BlockScope ss' []

execStmt (ForStmt ids vs body) m pm ss' = do 
                                        vss <- (getLists m pm ss' [vs])
                                        vss' <- over vss
                                        forStmt ids vss' body m pm ss'
                                        where
                                            getLists m pm ss (xs:[])  = do xss <- (evalList m pm ss xs)
                                                                           case xss of
                                                                                [(List list)] -> do return [(List list)]
                                                                                [(Map map)] -> do return [(List ( makeMap (M.toList map) ))]
                                            getLists m pm ss (xs:xss) = do xss' <- (evalList m pm ss xs) 
                                                                           xss'' <- (getLists m pm ss xss)
                                                                           return (xss' ++ xss'')

                                            makeMap :: [(Value, Value)] -> [Value]
                                            makeMap []     = []
                                            makeMap [x]    = [Pair x]
                                            makeMap (x:xs) = (Pair x) : makeMap xs

                                            forStmt ids vs body m pm ss' = do
                                                if length vs == 1
                                                then do
                                                    let nameCell = getNameCell ids (head vs)
                                                    case body of
                                                        (CondStmt st) -> do
                                                            (m',ss'',v) <- execBlock (Block [st]) m pm IterationScope ss' nameCell
                                                            return (m', ss'', Nothing)
                                                        (CondBlock block) -> do
                                                            (m',ss'',v) <- execBlock block m pm IterationScope ss' nameCell
                                                            return (m', ss'', Nothing)
                                                else do
                                                    if length vs > 0
                                                    then do
                                                        let nameCell = getNameCell ids (head vs)                                                        
                                                        case body of
                                                            (CondStmt st) -> do
                                                                (m',ss'',v) <- execBlock (Block [st]) m pm IterationScope ss' nameCell
                                                                forStmt ids (tail vs) body m' pm ss''
                                                            (CondBlock block) -> do
                                                                (m',ss'',v) <- execBlock block m pm IterationScope ss' nameCell
                                                                forStmt ids (tail vs) body m' pm ss''
                                                    else do
                                                        return (m, ss', Nothing)

execStmt (WhileStmt e body) m pm ss' =  --let ss' = (IterationScope (length ss):ss) in 
                                        repeatWhile body m pm ss'
                                    where
                                        repeatWhile body m pm ss' = 
                                                            do 
                                                                    v <- eval m pm ss' e
                                                                    let test = case makeBooleanFromValue v of
                                                                                    Left i -> error i
                                                                                    Right i -> i in 
                                                                        if test then do
                                                                                case body of
                                                                                    (CondStmt st) -> do 
                                                                                            (m',ss'',v) <- execBlock (Block [st]) m pm IterationScope ss' []
                                                                                            case v of 
                                                                                                Nothing -> repeatWhile body m' pm ss''
                                                                                                _ -> return $ (m',ss'',v)
                                                                                    (CondBlock block) -> do 
                                                                                            (m',ss'',v) <- execBlock block m pm IterationScope ss' []
                                                                                            case v of 
                                                                                                Nothing -> repeatWhile body m' pm ss''
                                                                                                _ -> return $ (m',ss'',v)
                                                                         else
                                                                            return (m, ss', Nothing) 

execStmt (SubCallStmt (SubprogCall (Ident i) as)) m pm ss = do  
                                                                arguments <- processSubArgs as [] m pm ss
                                                                selected <- return $ selectSubForCall i arguments pm
                                                                do
                                                                    case selected of
                                                                        Nothing -> error ("No subprogram found for call to " ++ i)
                                                                        Just sub -> do 
                                                                                execSubprogram m pm scopes sub arguments

execStmt (ReturnStmt e) m pm ss = do    v <- eval m pm ss e
                                        return $ (m',ss'', Just v)
                                                where (ss'', m') = case clearScopesUntilSub m ss of
                                                        Nothing -> error "Return called outside subprogram scope"
                                                        Just v' -> v'

-- | Produce a register value to store in memory from a struct declaration
makeRegister :: ProgramMemory -> StructIdentifier -> MemoryValue
makeRegister pm si = Register $ M.fromList (makeSetList pm sc)
    where
        makeSetList pm [] = []
        makeSetList pm ((n, t, mv):ds) = 
                                        case mv of
                                            Nothing -> (n, (t, def)) : makeSetList pm ds
                                                where def = case t of
                                                            GUserType t' -> makeRegister pm t'
                                                            _ -> Value $ defaultValue pm t
                                            Just v -> (n, (t, Value v)) : makeSetList pm ds
                                                where memval = case v of
                                                                Setter _ -> makeRegisterFromSetter pm (makeRegister pm t') v t'
                                                                    where (GUserType t') = t
                                                                _ -> Value v
        (_,sc) = fetchStructDecl pm si

-- | Interpret struct declaration
interpretStructDecl :: Memory -> ProgramMemory -> Scopes -> StructDecl -> IO (StructIdentifier, StructContent)
interpretStructDecl m pm ss decl@(Struct (GUserType  i) _) = do 
                                                            sc <- interpretStructDecl' m pm ss decl
                                                            return $ (i, sc)
    where
        interpretStructDecl' m pm ss (Struct t []) = return $ []
        interpretStructDecl' m pm ss (Struct t (d:ds)) = do
                                                            d' <- interpretVarDeclaration m pm ss d
                                                            remain <- interpretStructDecl' m pm ss (Struct t ds)
                                                            return $ d' ++ remain
            --case stmt of
                
-- | Interpret var declarations
interpretVarDeclaration :: Memory -> ProgramMemory -> Scopes -> VarDeclaration -> IO [(Name, GType, Maybe Value)]
interpretVarDeclaration m pm ss (VarDeclaration ns t es) = interpret m pm ss (VarDeclaration ns t es'')
    where 
        es'' = case fillReplicate ns es of
                    Nothing -> error "More expressions than identifiers in declaration"
                    Just l -> l
        interpret m pm ss (VarDeclaration [] t []) = do
                                                        return $ []
        interpret m pm ss (VarDeclaration ((Ident n):ns) t []) = do
                                                                remain <- interpret m pm ss (VarDeclaration ns t [])
                                                                return $ (n,t, Nothing) : remain
        interpret m pm ss (VarDeclaration ((Ident n):ns) t (e:es)) = do
                                                                v <- eval m pm ss e
                                                                remain <- interpret m pm ss (VarDeclaration ns t es)
                                                                return $ (n, t, Just v) : remain
                
-- | Given two lists, l1 and l2, error if |l1|<|l2|, and fill l2 with its last element if |l2|<|l1|
fillReplicate :: [a] -> [b] -> Maybe [b]
fillReplicate [] [] = Just []
fillReplicate (_:xs) [] = Just []
fillReplicate [] (_:ys) = Nothing
fillReplicate (_:xs'@(x:xs)) [y] = 
                case fillReplicate xs' [y] of
                    (Just r) -> Just $ y : r
                    Nothing -> Nothing
fillReplicate (x:xs) (y:ys) = 
                case fillReplicate xs ys of
                    (Just r) -> Just $ y : r
                    Nothing -> Nothing



-- | Clear until subprogram scope.
clearScopesUntilSub :: Memory -> Scopes -> Maybe (Scopes, Memory)
clearScopesUntilSub m ss = clearScopesUntilType m (SubScope (-1)) ss

-- | Clear until an iteration scope.
clearScopesUntilIter :: Memory -> Scopes -> Maybe (Scopes, Memory)
clearScopesUntilIter m ss = clearScopesUntilType m (IterationScope (-1)) ss

-- | Clear scopes until the type of scope s is reached.
clearScopesUntilType :: Memory -> Scope -> Scopes -> Maybe (Scopes, Memory)
clearScopesUntilType m _ [] = Nothing
clearScopesUntilType m _ [GlobalScope] = Nothing
clearScopesUntilType m t@(SubScope _) (s@(SubScope _):ss) = Just (ss, m')
    where m' = clearScope t m
clearScopesUntilType m t@(SubScope _) (_:ss) = clearScopesUntilType m' t ss
    where m' = clearScope t m
clearScopesUntilType m t@(IterationScope _) (s@(IterationScope _):ss) = Just (ss, m')
    where m' = clearScope t m
clearScopesUntilType m t (s@(BlockScope _):ss) = clearScopesUntilType m' t ss
    where m' = clearScope t m

-- | Executes a subprogram
execSubprogram :: Memory -> ProgramMemory -> Scopes -> (SubIdentifier, SubContent) -> ProcessedActualParams -> IO (Memory, Scopes, Maybe Value)
execSubprogram m pm ss sub@(ident,content@(_,_,block)) as = do
                                                                (m'',ss'',v) <- execBlock block m pm SubScope ss declarations
                                                                return (m'',ss'',v)
                                                        where declarations = prepareSubcallElabs m pm ss content as
                            
-- | Prepare elaboration for subprogram call.
-- Calls must be in the form: 
--      f(x1,x2,x3,...,xn,n1,n2,n3,...,nm)
-- where xi is an ordered parameter, and nj is a named parameter.
-- Cases follow this semantic:
--      - non single identifier parameter passing (expressions but identifiers alone): 1, [1,2], a+2...
--          - just take the value
--      - single identifier parameter passing: a,b,c...
--          - check if formal asks for a reference or a value and answers accordingly
--      - a named parameter: a=2, b=2,...
--          - remove parameter from parameters list
-- When actual arguments finalize, it is important to check optional formal parameters declarations
prepareSubcallElabs :: Memory -> ProgramMemory -> Scopes -> SubContent -> ProcessedActualParams -> [(Name, Cell)]
prepareSubcallElabs m pm ss ([], mt, b) [] = []
prepareSubcallElabs m pm ss ((f@(n,pt,mv)):fs, mt, b) [] = case mv of
                                                                Nothing -> prepareSubcallElabs m pm ss (fs, mt, b) []
                                                                Just v -> (n, (getType v, Value v)) : prepareSubcallElabs m pm ss (fs, mt, b) []
prepareSubcallElabs m pm ss (fs'@((f@(n,pt,mv)):fs), mt, b) ((a,t):as) = case a of
                                  Right v -> (n, (t, Value v)) : prepareSubcallElabs m pm ss (fs, mt, b) as
                                  Left ((Ident i), Left ci@(n',s')) -> case pt of 
                                                                (GRef _) -> (n, v') : prepareSubcallElabs m pm ss (fs, mt, b) as
                                                                    where v' = case fetchCellByScope m n' s' of
                                                                                    Left i -> error i
                                                                                    Right ci'@(t,v'') -> case v'' of 
                                                                                                Value _ -> (t,Ref ci)
                                                                                                Ref ci'' -> (t, Ref ci'')
                                                                (GType _) -> (n, (t, Value v')) : prepareSubcallElabs m pm ss (fs, mt, b) as
                                                                    where v' = case getVarScopeValue m n' s' of
                                                                                    Left i -> error i
                                                                                    Right v'' -> v''
                                  Left ((Ident i), Right (Just v)) ->   
                                                    if existsFormal i fs' then 
                                                        (i, (t, Value v)) : prepareSubcallElabs m pm ss (removeFormal i fs', mt, b) as
                                                    else 
                                                        error $ "Named parameter " ++ i ++ " doesnt match any unbound formal parameter in the call"

-- | Remove a formal parameter from a list of formal parameters
removeFormal :: Name -> [FormalParameter] -> [FormalParameter]
removeFormal _ [] = []
removeFormal n ((f@(n',_,_)):fs) = if n /= n' then f:removeFormal n fs else fs

-- | Tests if there exists a formal parameter with the given name
existsFormal :: Name -> [FormalParameter] -> Bool
existsFormal n = or . map (\(n',_,_) -> n == n')


-- | Process list of actual parameters from a subprogram call.
--
processSubArgs :: [SubprogArg] -> [Identifier] -> Memory -> ProgramMemory -> Scopes -> IO [(Either (Identifier, Either CellIdentifier (Maybe Value)) Value, GType)]
processSubArgs [] _ _ _ _ = return []
processSubArgs (a:as) ids m pm ss = case a of
                                       ArgExpr expr -> do   remaining <- processSubArgs as ids m pm ss
                                                            if ids == [] then
                                                                case expr of 
                                                                    ArithTerm (IdTerm id@(Ident i)) -> return $ (Left (id, Left ci), t): remaining
                                                                        where 
                                                                              (ci, (t,tc)) = case fetchVar m i ss of
                                                                                                Left err -> error err
                                                                                                Right cell -> cell
                                                                    _ -> do 
                                                                            ev <- eval m pm ss expr
                                                                            return $ (Right ev, getType ev) : remaining
                                                            else error "Optional parameter before ordered parameter"
                                       ArgIdentAssign (IdentAssign [i] expr) -> do
                                                                        ev <- eval m pm ss expr
                                                                        remaining <- processSubArgs as (i:ids) m pm ss
                                                                        if elem i ids then error "Multiple assignment to same parameter"
                                                                            else return $ (Left (i, Right (Just ev)), getType ev) : remaining
 
-- | Auxiliar for execting attribute statements
execAttrStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO Memory
execAttrStmt (AttrStmt (t:ts) (v:vs)) m pm ss = case t of
            (ArithTerm (IdTerm (Ident i))) ->   do
                                                    k <- eval m pm ss v
                                                    fetched <- return $ fetchVar m i ss
                                                    case fetched of
                                                        Left i -> error i
                                                        Right (_,(t',_)) -> 
                                                                do case k of
                                                                        setter@(Setter _) -> 
                                                                                case updateVar m i ss (makeCompatibleAssignTypes pm t' setter) of
                                                                                    Right m' -> return m'
                                                                                    Left i -> error i
                                                                        _ -> do    
                                                                                case updateVar m i ss (makeCompatibleAssignTypes pm t' k) of
                                                                                    Right m' -> return m'
                                                                                    Left i -> error i
            (ListAccess id@(ArithTerm (IdTerm (Ident i))) index) -> do  v1 <- eval m pm ss id
                                                                        v2 <- eval m pm ss index
                                                                        v3 <- eval m pm ss v
                                                                        let index' = case v2 of
                                                                                (Integer int) -> int
                                                                                _ -> error "List index must be an integer." 
                                                                            k = case v1 of
                                                                                    (List xs) -> List (setElemList xs index' v3)
                                                                                    _ -> error "You must access a list." 
                                                                            t' = getType k in
                                                                                case updateVar m i ss (makeCompatibleAssignTypes pm t' k) of
                                                                                    Right m' -> return m'
                                                                                    Left i -> error i 
            (DictAccess id@(ArithTerm (IdTerm (Ident i))) index) -> do  
                                                                        v1 <- eval m pm ss id
                                                                        v2 <- eval m pm ss v
                                                                        v3 <- eval m pm ss index
                                                                        do 

                                                                            let     kt = (\(GDict kt _)->kt) (getType v1)
                                                                                    index' = case getType v3 of
                                                                                                kt -> v3
                                                                                                _ -> error "Incompatible index type."
                                                                                    k = case v1 of
                                                                                            (Map m') -> Map (M.insert index' v2 m')
                                                                                            _ -> error "You must access a dictionary." 
                                                                                    t' = getType k in
                                                                                        case updateVar m i ss (makeCompatibleAssignTypes pm t' k) of
                                                                                                    Right m' -> return m'
                                                                                                    Left i -> error i

                                                                                                        
-- |From value, guaarantee boolean value
makeBooleanFromValue :: Value -> Either String Bool
makeBooleanFromValue (Bool v) = Right v
makeBooleanFromValue _        = Left "Expected boolean value"

-- | Check types for compatibility
checkCompatType :: GType -> GType -> Bool
checkCompatType t t' = if t == t' then True
                    else case (t,t') of
                        (GFloat, GInteger)  -> True
                        (GList _, GEmpty)   -> True
                        (GEmpty ,GList _)   -> True
                        (GList l, GList l') -> checkCompatType (GList l) l'
                        (GList l, GList l') -> checkCompatType (l) $ GList l'
                        _                   -> False


-- | Given type t and value v, return (t,v) if they are compatible.
makeCompatibleAssignTypes :: ProgramMemory -> GType -> Value -> (GType, MemoryValue)
makeCompatibleAssignTypes pm t@(GList _) v@(List []) = (t, Value v)
makeCompatibleAssignTypes pm t@(GUserType u) v@(Setter m) = (t, makeRegisterFromSetter pm (makeRegister pm u) v u)
makeCompatibleAssignTypes pm t v = (t, Value $ coerceAssignByType t v)

coerceAssignByType :: GType -> Value -> Value
coerceAssignByType GFloat i'@(Integer i) = coerce i'
coerceAssignByType t v = if checkCompatType t t' then v
                   else error ("Incompatible types " ++ show t ++ " and " ++ show t')
                    where
                        t' = getType v

-- | Given a setter, a user type name and a register (memory value), produce a new register
-- The value is the setter.
makeRegisterFromSetter :: ProgramMemory -> MemoryValue -> Value -> StructIdentifier -> MemoryValue
makeRegisterFromSetter pm (Register r) (Setter msetter) si = Register $ updateRegister (M.toList msetter) r
        where
                updateRegister :: [(String, Value)] -> M.Map Name Cell -> M.Map Name Cell
                updateRegister [] m = m
                updateRegister ((s,v):ss) m 
                            | M.member s m = case v of
                                                (Setter setter') -> updateRegister ss m''
                                                    where m'' = M.update (\k -> Just (t',makeRegisterFromSetter pm r' (Setter setter') si)) s m
                                                          (t',r') = m M.! s
                                                _ -> updateRegister ss m''  
                                                    where (t',_) = m M.! s
                                                          m'' = M.update (\k -> Just (makeCompatibleAssignTypes pm t' v)) s m  
                            | otherwise = error $ "Type " ++ si ++ " doesn't have field " ++ s     

-- | Set the i-element of a list.
setElemList :: Integral i => [a] -> i -> a -> [a]
setElemList [] i k 
    | i >= 0 = error "Index out of range"
    | otherwise = []
setElemList (x:xs) 0 k = k : xs
setElemList (x:xs) i k = x : setElemList xs (i-1) k

-- |Executes a declaration statement.
varDeclStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO Memory
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t [])) m pm ss = do 
                                                            case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes pm t (defaultValue pm t)) m of
                                                                (Left i) -> error i
                                                                (Right m') -> varDeclStmt (DeclStmt (VarDeclaration xs t [])) m' pm ss 
varDeclStmt (DeclStmt (VarDeclaration [] t [])) m pm ss = do 
                                                                return m
varDeclStmt (DeclStmt (VarDeclaration [] t (_:es))) m pm ss =     do 
                                                                    error "Too many expressions in right side."
varDeclStmt (DeclStmt (VarDeclaration (x:xs'@(y:xs)) t (e:[]))) m pm ss = do 
                                                                do
                                                                    v <- eval m pm ss e
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes pm t v) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs' t (e:[]))) i pm ss
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t (e:es))) m pm ss = do 
                                                                    v <- eval m pm ss e
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes pm t v) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs t es)) i pm ss

-- | Interpret subprogram declaration
interpretSubDeclaration :: Subprogram -> Memory -> ProgramMemory -> Scopes -> IO (SubIdentifier, SubContent)
interpretSubDeclaration (Subprogram (Ident i) ps t b) m pm ss = do 
                                                                    formals <- formalParams ps
                                                                    return $ ((i, paramTypes ps), (formals, t, b)) 
    where   
            paramTypes [] = []
            paramTypes ((ParamDeclaration ids gt _):ps) = replicate (length ids) gt ++ paramTypes ps

            formalParams :: [ParamDeclaration] -> IO [(String, GParamType, Maybe Value)]
            formalParams [] = return []
            formalParams (p:ps) = do interp <- (interpretParamDeclaration p m pm ss) 
                                     remain <- formalParams ps
                                     return $ interp ++ remain

-- | Interpret formal parameters
interpretParamDeclaration :: ParamDeclaration -> Memory -> ProgramMemory -> Scopes -> IO [(String, GParamType, Maybe Value)]
interpretParamDeclaration pd@(ParamDeclaration is _ es) m pm ss 
    | length es > length is = error "Too many default values"
    | otherwise = interpretParamDeclaration' pd m pm ss
    where
        interpretParamDeclaration' (ParamDeclaration [] _ []) _ _ _ = return []
        interpretParamDeclaration' (ParamDeclaration [(Ident i)] gt [e]) m pm ss = 
                                                        do  v <- eval m pm ss e 
                                                            return [(i, gt, Just v)]
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt []) m pm ss = 
                                                        do  remain <- interpretParamDeclaration (ParamDeclaration is gt []) m pm ss
                                                            return $ (i, gt, Nothing) : remain
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt [e]) m pm ss = 
                                                        do  v <- eval m pm ss e 
                                                            remain <- interpretParamDeclaration (ParamDeclaration is gt [e]) m pm ss
                                                            return $ (i, gt, Just v) : remain
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt (e:es)) m pm ss = 
                                                        do  v <- eval m pm ss e 
                                                            remain <- interpretParamDeclaration (ParamDeclaration is gt es) m pm ss
                                                            return $ (i, gt, Just v) : remain
 --Get type of a List and make coercion if needed                
getListType :: [Value] -> GType
getListType [x]      = getType x
getListType (x:y:xs) = case getType x of
                        GInteger -> case getType y of
                                     GInteger -> getListType (y:xs)
                                     GFloat -> GFloat
                        GFloat   -> GFloat
                        any      -> any 
               
-- | Return the type of a given Gryph value.
getType :: Value -> GType
getType (Integer i)                   = GInteger
getType (Float f)                     = GFloat
getType (String s )                   = GString
getType (Char c)                      = GChar
getType (Bool b)                      = GBool
getType (List [])                     = GEmpty
getType (List (x:xs))                 = GList (getListType (x:xs))
getType (Map (m))                     = GDict ( getType (head (M.keys m))) ( getType (head (M.elems m)))
getType (Pair (v1,v2))                = GPair (getType v1) (getType v2 )
getType (Triple (v1,v2, v3))          = GTriple (getType v1) (getType v2 ) (getType v3)
getType (Quadruple (v1,v2, v3, v4))   = GQuadruple (getType v1) (getType v2 ) (getType v3) (getType v4)

-- | Return the type dictionary keys
getKeyType :: Value -> GType
getKeyType (Map m) = getType (head (M.keys m))

-- | Return the type of dictionary values
getValueType :: Value -> GType 
getValueType (Map m) = getType (head (M.elems m))

coerce:: Value -> Value
coerce (Integer x) = (Float (fromInteger x))
coerce v = v

-- | Evaluates a list of expressions
evalList :: Memory -> ProgramMemory -> Scopes -> [ArithExpr] -> IO [Value]
evalList m pm ss [x]      =  do {x' <- eval m pm ss x ; return $ [x']}
evalList m pm ss (x:y:xs) =  do 
                                  z  <- eval m pm ss x
                                  y' <- eval m pm ss y
                                  if not( checkCompatType (getType y') ( getType z) || checkCompatType (getType z) (getType y') ) then error "Type mismatch in List "
                                    else do
                                        l <- evalList m pm ss  (y:xs)
                                        return (z:l) 

-- | Evaluates a tuple
evalTuple :: Memory -> ProgramMemory -> Scopes -> [ArithExpr] -> IO [Value]
evalTuple m pm ss [x] = do {x' <-eval m pm ss x; return $ [x']}
evalTuple m pm ss (x:xs) = do   z <- eval m pm ss x
                                t <- evalTuple m pm ss xs
                                return $ (z:t)

-- | Evaluates a dictionary
evalDict :: Memory -> ProgramMemory -> Scopes -> [DictEntry] -> M.Map Value Value -> IO (M.Map Value Value)
evalDict m pm ss [] m1                   = return $ M.empty
evalDict m pm ss ((k1,v1):(k2,v2):xs) m1 = 
                                         do     ek1 <- eval m pm ss k1
                                                ek2 <- eval m pm ss k2
                                                ev1 <- eval m pm ss v1
                                                ev2 <- eval m pm ss v2
                                                if (getType ek1) == (getType ek2) && (getType ev1 == getType ev2)
                                                 then do
                                                        d <- evalDict m pm ss ((k2,v2):xs) m1
                                                        return $ M.insert ek1 ev1 d
                                                else error "Dict Type mismatch"
evalDict m pm ss ((k,v):xs) m1           = 
                                        do      k' <- eval m pm ss k
                                                v' <- eval m pm ss v
                                                d <- evalDict m pm ss xs m1
                                                return $ M.insert k' v' d

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
defaultValue pm (GUserType u) = makeSetterFromDeclaration pm sc
    where (si, sc) = fetchStructDecl pm u

-- | Register to Setter
makeSetterFromDeclaration :: ProgramMemory -> StructContent -> Value
makeSetterFromDeclaration pm scs'@((n,t,mv):scs) = Setter (makeMap scs')
        where 
                makeMap :: StructContent -> M.Map String Value
                makeMap [] = M.empty
                makeMap scs'@((n,t,mv):scs) = M.insert n v' m'
                    where   m' = makeMap scs
                            v' = case mv of
                                    Nothing -> defaultValue pm t
                                    Just v'' -> v'' 

-- | Binary operation evaluator
evalBinOp ::Memory -> ProgramMemory -> Scopes -> ArithExpr ->( Value -> Value -> Value )-> IO Value
evalBinOp m pm ss (ArithBinExpr _  e1 e2) f = 
                                            do
                                                v1 <- eval m pm ss e1
                                                v2 <- eval m pm ss e2
                                                k <- eval m pm ss e2
                                                case v1 of
                                                     l1@(List (x:xs)) -> if (getType k == getType x) then return $ f l1 k
                                                                         else error "Type mismatch operation"
                                                     k -> case v2 of 
                                                           l2@(List (x:xs) ) -> if (getType k == getType x) then return $ f l2 k 
                                                                         else error "Type mismatch  operation"
                                                           x -> return $ f k  x
                                                           --x -> if (getType k == getType x) then return $ f k  x
                                                             --            else error "Type mismatch  operation"
-- | Convert a string to other type
fromString::  String  -> GType -> Value
fromString s (GInteger )        = Integer (read s::Integer)
fromString s (GFloat  )         = Float (read s::Double)
fromString s (GChar )           = Char (read s::Char)
fromString s (GBool )           = Bool (read s::Bool)
fromString _ _                  = error "No parser from String"


cast:: Value -> GType -> Value
cast v1 g@GString     =  String (show v1)
cast v1 g@GInteger    = case v1 of
                         Float f   -> Integer ( floor f)
                         Bool  b   -> if b then Integer 1 else Integer 0
                         String s  -> fromString s g
                         _         -> error "No cast avaliable"
cast v1 g@GFloat      = case v1 of
                         Integer i -> Float (fromInteger i)
                         String s  -> fromString s g
                         _         -> error "No cast avaliable"
cast v1 g@GBool       = case v1 of
                         Integer i -> if i /= 0 then Bool True else Bool False
                         Float   f -> if f /= 0 then Bool True else Bool False
                         String  s -> fromString s g
                         _         -> error "No cast avaliable"

cast v1 g@(GList GFloat)  = case v1 of 
                             List [] -> List []
                             l@(List (x:xs)) -> List ((cast x GFloat):r )
                              where List r = (cast (List xs) (GList GFloat) )
                             _               -> error "No cast avaliable"
cast v1 g@(GList GInteger)  = case v1 of 
                               List [] -> List []
                               l@(List (x:xs)) -> List ((cast x GInteger):r )
                                where List r = (cast (List xs) (GList GInteger) )
                               _               -> error "No cast avaliable"
cast v1 g@(GList GBool)  = case v1 of 
                               List [] -> List []
                               l@(List (x:xs)) -> List ((cast x GBool):r )
                                where List r = (cast (List xs) (GList GBool) )
                               _               -> error "No cast avaliable"
cast v1 g@(GList GString)  = case v1 of 
                               List [] -> List []
                               l@(List (x:xs)) -> List ((cast x GString):r )
                                where List r = (cast (List xs) (GList GString) )
                               _               -> error "No cast avaliable"
cast _   _          =  error "Unknown type"

-- | Main expression evaluator
eval :: Memory -> ProgramMemory -> Scopes -> ArithExpr -> IO Value
eval m pm ss (ArithTerm (LitTerm (Lit v)))      = return v
eval m pm ss (ArithUnExpr MinusUnOp e)          = do {v <- eval m pm ss e ; return $ minusUn v}
eval m pm ss (ArithUnExpr PlusUnOp e)           = do {v <- eval m pm ss e; return $ plusUn v}
eval m pm ss (ArithUnExpr NotUnOp e)            = do {v <- eval m pm ss e; return $ not' v}
eval m pm ss (ArithBinExpr MinusBinOp  e1 e2)   = evalBinOp m pm ss (ArithBinExpr MinusBinOp e1 e2) minusBin
eval m pm ss (ArithBinExpr PlusBinOp  e1 e2)    = evalBinOp m pm ss (ArithBinExpr PlusBinOp e1 e2) plusBin
eval m pm ss (ArithBinExpr TimesBinOp  e1 e2)   = evalBinOp m pm ss (ArithBinExpr TimesBinOp e1 e2) timesBin
eval m pm ss (ArithBinExpr DivBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr DivBinOp e1 e2) divBin
eval m pm ss (ArithBinExpr ExpBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr ExpBinOp e1 e2) expBin
eval m pm ss (ArithBinExpr ModBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr ModBinOp e1 e2) modBin
eval m pm ss (ExprLiteral (ListLit [] ))        = return $ List []
eval m pm ss (ExprLiteral (ListLit es ))        = do {v <- evalList m pm ss es; if getListType v == GFloat then return $List (map coerce v)  else  return $ List v}
eval m pm ss (ExprLiteral (DictLit de))         = do {v <- evalDict m pm ss de M.empty ; return $ Map v}
eval m pm ss (ArithEqExpr Equals e1 e2)         = do {v1 <- eval m pm ss e1; v2 <- eval m pm ss e2; return $ Bool (v1 == v2)}
eval m pm ss (ArithEqExpr NotEquals e1 e2)      = do {v1 <- eval m pm ss e1; v2 <- eval m pm ss e2; return $ Bool (v1 /= v2)} 
eval m pm ss (ArithRelExpr Greater e1 e2)       = do {v1 <- eval m pm ss e1; v2 <- eval m pm ss e2; return $ Bool (v1 > v2)}  
eval m pm ss (ArithRelExpr GreaterEq e1 e2)     = do {v1 <- eval m pm ss e1; v2 <- eval m pm ss e2; return $ Bool (v1 >= v2)}  
eval m pm ss (ArithRelExpr Less e1 e2)          = do {v1 <- eval m pm ss e1; v2 <- eval m pm ss e2; return $ Bool (v1 < v2)}   
eval m pm ss (ArithRelExpr LessEq e1 e2)        = do {v1 <- eval m pm ss e1; v2 <- eval m pm ss e2; return $ Bool (v1 <= v2)}    
eval m pm ss (LogicalBinExpr And e1 e2)         = 
                                                do      v1 <- eval m pm ss e1
                                                        v2 <- eval m pm ss e2  
                                                        case v1 of
                                                            Bool b1 -> case v2 of
                                                                        Bool b2 -> return $ Bool ( b1 && b2)
                                                                        _ -> error "And operator rhs type error"
                                                            _ -> error "And operator lhs type error"
eval m pm ss (LogicalBinExpr Or e1 e2)          = 
                                                do      v1 <- eval m pm ss e1
                                                        v2 <- eval m pm ss e2  
                                                        case v1 of
                                                            Bool b1 -> case v2 of
                                                                        Bool b2 -> return $ Bool ( b1 || b2)
                                                                        _ -> error "And operator rhs type error"
                                                            _ -> error "And operator lhs type error "
eval m pm ss (LogicalBinExpr Xor e1 e2)         = 
                                                do      v1 <- eval m pm ss e1
                                                        v2 <- eval m pm ss e2  
                                                        case v1 of
                                                            Bool b1 -> case v2 of
                                                                        Bool b2 -> return $ Bool (((not b1) &&  b2 ) || (b1 && (not b2)))
                                                                        _ -> error "And operator rhs type error"
                                                            _ -> error "And operator lhs type error "
eval m pm ss (CastExpr e1 g)                    =
                                                do      v1 <- eval m pm ss e1 
                                                        return $ cast v1 g 


eval m pm ss (ExprLiteral (TupleLit te))        = 
                                                do
                                                    l <- evalTuple m pm ss te
                                                    return $ if length l == 2 then Pair ((l !! 0), (l !! 1))
                                                       else if length l == 3 then Triple ((l !! 0), (l !! 1), (l !! 2))
                                                            else if length  l == 4 then Quadruple ((l !! 0), (l !! 1), (l !! 2), (l !! 3))
                                                             else error "Limit of Quadruples"
eval m pm ss (ListAccess e1 e2 )                = 
                                                do
                                                    v1 <- eval m pm ss e1
                                                    v2 <- eval m pm ss e2
                                                    case v1 of
                                                        (List l) -> case v2 of
                                                                     Integer i ->  return $ l !! (fromIntegral i)
                                                                     _ -> error "Access List mismatch"
                                                        _ -> error "Access List mismatch"
eval m pm ss (DictAccess e1 e2)                 = 
                                                do
                                                    v1 <- eval m pm ss e1
                                                    k <- eval m pm ss e2
                                                    case v1 of
                                                        d@(Map ma) -> if getKeyType d == getType k then case  M.lookup k ma of
                                                                                                              Nothing -> error "No key on Dict "
                                                                                                              Just k  -> return $ k
                                                                     else error "Access Dict with invalid key type"
                                                        _ -> error "Access on Dict type mismatch"
eval m pm ss (TupleAccess e1 e2)                = 
                                                do
                                                    v1' <- eval m pm ss e1
                                                    v2' <- eval m pm ss e2
                                                    case v1' of
                                                        Pair (v1, v2)     -> case v2' of
                                                                            Integer 0 -> return $ v1
                                                                            Integer 1 -> return $ v2
                                                                            _         -> error "Acessing Pair"
                                                        Triple (v1,v2,v3)-> case v2' of
                                                                            Integer 0 -> return $ v1
                                                                            Integer 1 -> return $ v2
                                                                            Integer 2 -> return $ v3
                                                                            _         -> error "Acessing Pair"
                                                        Quadruple (v1,v2,v3,v4)-> case v2' of
                                                                            Integer 0 -> return $ v1
                                                                            Integer 1 -> return $ v2
                                                                            Integer 2 -> return $ v3
                                                                            Integer 3 -> return $ v4
                                                                            _         -> error "Acessing Pair"
                                                         
                                                        _ -> error "Tuple error " 

eval m pm ss (ArithBinExpr PlusPlusBinOp e1 e2) = 
                                                do
                                                    v1 <- eval m pm ss e1
                                                    v2 <- eval m pm ss e2
                                                    case v1 of
                                                        l1@(List []) -> case v2 of 
                                                                         l2@(List [])     -> return $ plusPlusBin l1 l2
                                                                         l2@(List (x:xs)) -> return $ plusPlusBin l1 l2  
                                                                         k                -> return $ plusPlusBin k l1
                                                        l1@(List (x:xs)) -> case v2 of
                                                                                l2@(List (y:ys)) -> if (getType x == getType y) then return $ plusPlusBinList l1 l2
                                                                                                    else return $ plusPlusBin l1 l2
                                                                                k -> if (getType k == getType x) then return $ plusPlusBin l1 k
                                                                                     else error "Type mismatch ++ opeator "
                                                        k -> case v2 of 
                                                                l2@(List [])     -> return $ plusPlusBin l2 k
                                                                l2@(List (y:ys)) -> if (getType k == getType y) then return $ plusPlusBin k l2
                                                                                else error "Type mismatch ++ operator "

eval m pm ss (ArithTerm (IdTerm (Ident i))) = case fetchVarValue m i ss of
                                                Left i -> error i
                                                Right i -> return $ i

eval m pm ss (ArithTerm (SubcallTerm (SubprogCall (Ident i) as))) = 
                                                do  arguments <- processSubArgs as [] m pm ss
                                                    selected <- return $ selectSubForCall i arguments pm
                                                    case selected of
                                                        Nothing -> error ("No subprogram found for call to " ++ i)
                                                        Just sub -> do
                                                                        (m',ss',v) <- execSubprogram m pm scopes sub arguments
                                                                        do
                                                                            case v of
                                                                                Nothing -> error "No return from subprogram call"
                                                                                Just v -> return v

eval m pm ss (ExprLiteral (ListCompLit lc)) = do {r <- forListComp m pm ss lc ; return  $ List r}

eval m pm ss (StructInitExpr (StructInit ias)) = 
    do case ias of
            [] -> return $ Setter M.empty
            ((IdentAssign [(Ident i)] e):ias') -> do
                                            Setter remain <- eval m pm ss (StructInitExpr (StructInit ias'))
                                            v <- eval m pm ss e
                                            return $ Setter (M.insert i v remain)


-- | The concatenation operation on lists
plusPlusBinList :: Value -> Value -> Value
plusPlusBinList (List xs'@(x:xs)) (List ys'@(y:ys)) = List (xs' ++ ys')

plusPlusBin :: Value -> Value -> Value
plusPlusBin k (List []) = (List [k])
plusPlusBin (List []) k = (List [k])
plusPlusBin k (List xs'@(x:xs)) = if not (checkCompatType (getType k) tl) || not (checkCompatType tl (getType k)) then error "Type mismatch in operation ++"
                           else List (k:xs')
                                where tl = getListType xs' 
plusPlusBin (List xs'@(x:xs)) k = if getType (k) /= tl then error "Type mismatch in operation ++"
                           else List (xs' ++ [k])
                                where tl = getType x

modBin ::  Value -> Value -> Value
modBin (Integer i) (Integer j) = (Integer (i `mod`  j)) 
modBin (List l)  i             =   List ( map ((flip modBin) i) l)
modBin  i (List l)             =   List ( map (modBin i) l)

expBin ::  Value -> Value -> Value
expBin (Integer i) (Integer j) = (Integer (i ^ j)) 
expBin (Float f) (Integer i )  = ( Float (f ** (fromInteger i)))
expBin (Integer i) ( Float f)  = ( Float ((fromInteger i) ** f))  
expBin (Float f1) (Float f2)   = ( Float (f1 ** f2))
expBin (List l)  i             =   List ( map ((flip expBin) i) l)
expBin  i (List l)             =   List ( map (expBin i) l)

divBin ::  Value -> Value -> Value
divBin (Integer i) (Integer j) = (Integer (i `div` j)) 
divBin (Float f) (Integer i )  = ( Float (f / (fromInteger i)))
divBin (Integer i) ( Float f)  = ( Float ((fromInteger i) / f))  
divBin (Float f1) (Float f2)   = ( Float (f1 / f2))
divBin (List l)  i             =   List ( map ((flip divBin) i) l)
divBin  i (List l)             =   List ( map (divBin i) l)

timesBin ::  Value -> Value -> Value
timesBin (Integer i) (Integer j) = (Integer (i*j)) 
timesBin (Float f) (Integer i )  = ( Float (f * (fromInteger i)))
timesBin (Integer i) ( Float f)  = ( Float ((fromInteger i) * f))  
timesBin (Float f1) (Float f2)   = ( Float (f1 * f2))
timesBin (List l)  i             =   List ( map (timesBin i) l)
timesBin  i (List l)             =   List ( map (timesBin i) l)

plusBin ::  Value -> Value -> Value
plusBin (Integer i) (Integer j)  = Integer (i+j) 
plusBin (Float f) (Integer i )   =  Float (f + (fromInteger i))
plusBin (Integer i) ( Float f)   =  Float ((fromInteger i) +f)  
plusBin (Float f1) (Float f2)    =  Float (f1 + f2)
plusBin (List l)  i              =  List ( map (plusBin i) l)
plusBin  i (List l)              =  List ( map (plusBin i) l)
plusBin  (String s1) (String s2) =  String (s1 ++ s2)

minusBin ::  Value -> Value -> Value
minusBin (Integer i) (Integer j) = (Integer (i-j)) 
minusBin (Float f) (Integer i )  = ( Float (f - (fromInteger i)))
minusBin (Integer i) ( Float f)  = ( Float ((fromInteger i) - f))  
minusBin (Float f1) (Float f2)   = ( Float (f1 - f2))
minusBin (List l)  i             =   List ( map ((flip minusBin) i) l)
minusBin  i (List l)             =   List ( map (minusBin i) l)

plusUn :: Value -> Value
plusUn (Integer i) = Integer i
plusUn (Float i) = Float i
plusUn _ = error "Type error Unary (+) operator "

minusUn :: Value -> Value
minusUn (Integer i) = Integer (-i)
minusUn (Float i) = Float (-i)
minusUn _ = error "Type error Unary (-)  operator " 

not' :: Value -> Value
not' (Bool b) = Bool (not b)
not' _ = error "Type error Unary (not) operator "

--------------------------------------------------------------
-- |List Comprehension

forListComp :: Memory -> ProgramMemory -> Scopes -> ListComp -> IO [Value]
forListComp m pm ss lc = do (exp, id, vs'@(v:vs), when_exp) <- evalListComp m pm ss lc
                            let (Right m') = elabVars m (getNameCell id v) (head ss)
                            r <- forListComp' m' pm ss exp id vs' when_exp
                            return r

forListComp' :: Memory -> ProgramMemory -> Scopes -> ArithExpr -> [Identifier] -> [Value] -> Maybe ArithExpr -> IO [Value]
forListComp' m pm ss exp id [v] when_exp = do
                                                (Right m') <- updateListIds m ss id v
                                                case when_exp of
                                                    Nothing -> do
                                                        e <- eval m' pm ss exp 
                                                        return [e]
                                                    Just when_exp -> do
                                                        success <- (eval m' pm ss when_exp)
                                                        if success == (Bool True)
                                                        then do
                                                            e <- eval m' pm ss exp 
                                                            return [e]
                                                        else do 
                                                            return []

forListComp' m pm ss exp id (v:vs) when_exp = do
                                                (Right m') <- updateListIds m ss id v
                                                case when_exp of
                                                    Nothing -> do
                                                        e <- eval m' pm ss exp
                                                        r <- forListComp' m pm ss exp id vs when_exp
                                                        return (e : r)
                                                    Just when_exp -> do
                                                        success <- (eval m' pm ss when_exp)
                                                        if success == (Bool True)
                                                        then do
                                                            e <- eval m' pm ss exp
                                                            r <- forListComp' m pm ss exp id vs (Just when_exp)
                                                            return (e : r)
                                                        else do 
                                                            r <- forListComp' m pm ss exp id vs (Just when_exp)
                                                            return r

evalListComp :: Memory -> ProgramMemory -> Scopes -> ListComp -> IO (ArithExpr, [Identifier], [Value], Maybe ArithExpr)
evalListComp m pm ss (ListComp expression (ForIterator is xs when_exp ) ) = do
        let new_xs = replicateList ((length is) - (length xs)) xs
        xss <- (getLists m pm ss [new_xs])
        xss' <- (over xss)
        if when_exp == []
        then do
            return (expression, is, xss', Nothing)
        else do
            return (expression, is, xss', (Just (head when_exp)))            
    where getLists m pm ss (xs:[])  = do xss <- (evalList m pm ss xs)
                                         return xss
          getLists m pm ss (xs:xss) = do xss' <- (evalList m pm ss xs) 
                                         xss'' <- (getLists m pm ss xss)
                                         return (xss' ++ xss'')
          replicateList 0 xs = xs
          replicateList n xs | n < 0     = error "There aren't enough identifiers." 
                             | otherwise = replicateList (n-1) xs ++ [last xs]

over :: [Value] -> IO [Value]
over []       = do return [] 
over [(List xs)]       = do return [(List [x]) | x <- xs]
over (List [x]: xss)   = do xss' <- (over xss)
                            xss'' <- (join x xss')
                            return xss''
over (List (x:xs):xss) = do xss'  <- (over xss)
                            xss'' <- (join x xss') 
                            xss''' <- (over ((List xs): xss))
                            return (xss'' ++ xss''')

join :: Value -> [Value] -> IO [Value]
join v []     = do return []
join v [(List xs)]     = do return [ List (v : xs) ]
join v ((List xs):xss) = do xss' <- (join v xss)
                            return ((List (v : xs)) : xss')

getNameCell :: [Identifier] -> Value -> [(Name,Cell)]
getNameCell [(Ident id)] (List [v]) = [ (id, ((getType v), (Value v)) ) ]
getNameCell ((Ident id):ids) (List (v:vs)) = (id, ((getType v), (Value v)) ) : (getNameCell ids (List vs))                                          

updateListIds :: Memory -> Scopes -> [Identifier] -> Value -> IO (Either String Memory)
updateListIds m ss [(Ident id)] (List [v])        = do 
                                                        let r = updateVar m id ss ((getType v), (Value v))
                                                        return r
updateListIds m ss ((Ident id):ids) (List (v:vs)) = do
                                                        let (Right m') = updateVar m id ss ((getType v), (Value v))
                                                        r <- updateListIds m' ss ids (List vs)
                                                        return r
