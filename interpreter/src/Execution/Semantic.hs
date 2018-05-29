module Execution.Semantic where

import Syntactic.Values
import Syntactic.Syntax
import Execution.Memory
--import Syntactic.Parser 
import qualified Data.Map as M


type Filename = String

-- |Scopes in the execution.
scopes :: Scopes
scopes = ["global"]

-- |Execute a program represented as a list of program units.
exec :: Memory -> ProgramMemory -> Scopes -> [ProgramUnit] -> IO() 
exec m pm ss [] = return ()
exec m pm ss (u:us) = do
                        (m', pm', ss') <- execUnit u m pm ss
                        exec m' pm' ss' us 

-- |Executes a program unit.
execUnit :: ProgramUnit -> Memory -> ProgramMemory -> Scopes -> IO (Memory, ProgramMemory, Scopes)
execUnit (SubprogramDecl sub) m pm ss = do 
                                            pm' <- execSubDecl sub m pm ss
                                            return (m, pm', ss)
execUnit (StructDecl struct) m pm ss = 
                                do
                                    execStructDecl struct m pm
                                    return (m,pm,ss)

execUnit (Stmt stmt) m pm ss = do 
                                    (m', ss') <- execStmt stmt m pm ss
                                    return (m', pm, ss')

-- |Executes a subprogram declaration.
execSubDecl :: Subprogram -> Memory -> ProgramMemory -> Scopes -> IO (ProgramMemory)
execSubDecl s m pm ss = do case declareSubprogram si sc pm of
                                Left i -> error i
                                Right pm' -> return pm'
                        where (si, sc) = interpretSubDeclaration s m pm ss

-- |Executes a struct declaration.
execStructDecl :: StructDecl -> Memory -> ProgramMemory -> IO ()
execStructDecl s m = undefined

-- |Executes a block of statement, creating a new scope. When finish, clear the scope.
execBlock :: Block -> Memory -> ProgramMemory -> Scopes -> IO (Memory, Scopes)
execBlock (Block []) m pm ss = return (m, ss)
execBlock (Block (st:sts)) m pm ss = do
                                    (m',ss') <- execStmt st m pm ss
                                    execBlock (Block sts) m' pm ss' 

-- |Executes any statement.
execStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO (Memory, Scopes)
execStmt d@(DeclStmt _) m pm ss = do
                            m' <- varDeclStmt d m pm ss
                            return (m', ss)
execStmt a@(AttrStmt _ _) m pm ss = do
                            m' <- execAttrStmt a m pm ss
                            return (m', ss)
execStmt (PrintStmt e) m pm ss = do
                                putStrLn (show (eval m pm ss e))
                                return (m, ss)
execStmt (ReadStmt i) m pm ss = do
                                value <- getLine
                                case updateVar m ((\(Ident i) -> i) i) ss (makeCompatibleAssignTypes GString (String value)) of
                                    Left e -> error e
                                    Right m' -> return (m', ss)
                                

execStmt (IfStmt e (IfBody ifbody) elsebody) m pm ss = let ss' = (show (length ss):ss) in
                                                        if test then
                                                            case ifbody of
                                                                (CondStmt st) -> do 
                                                                                    (m',ss'') <- execStmt st m pm ss'
                                                                                    return (clearScope (head ss') m', tail ss')
                                                                (CondBlock block) -> do 
                                                                                    (m',ss'') <- execBlock block m pm ss'
                                                                                    return (clearScope (head ss') m', tail ss')
                                                        else
                                                            case elsebody of
                                                                NoElse -> do return (m,ss)
                                                                ElseBody (CondStmt st) -> do 
                                                                                            (m',ss'') <- execStmt st m pm ss
                                                                                            return (clearScope (head ss') m', tail ss')
                                                                ElseBody (CondBlock block) -> do 
                                                                                            (m',ss'') <- execBlock block m pm ss
                                                                                            return (clearScope (head ss') m', tail ss')
                                        where test = case makeBooleanFromValue (eval m pm ss e) of
                                                        Left i -> error i
                                                        Right i -> i

execStmt (WhileStmt e body) m pm ss =  let ss' = (show (length ss):ss) in repeatWhile body m pm ss'
                                    where
                                        repeatWhile body m pm ss' = if test then do
                                                                        case body of
                                                                            (CondStmt st) -> do 
                                                                                                (m',ss'') <- execStmt st m pm ss'
                                                                                                repeatWhile body (clearScope (head ss') m') pm ss''
                                                                            (CondBlock block) -> do 
                                                                                                (m',ss'') <- execBlock block m pm ss'
                                                                                                repeatWhile body (clearScope (head ss') m') pm ss''
                                                                 else return (clearScope (head ss') m, tail ss')
                                                where test = case makeBooleanFromValue (eval m pm ss' e) of
                                                                Left i -> error i
                                                                Right i -> i
execStmt (SubCallStmt (SubprogCall i as)) m pm ss = do 
                                                        print (show (typeArgs as m pm ss))
                                                        return (m, ss)

type ActualParamTypes = [GType]
type ProcessedActualParams = [(Either (Identifier, Either CellIdentifier (Maybe Value)) Value, GType)]

execSubprogram :: Identifier -> ProcessedActualParams -> IO (Memory, Scopes, Maybe Value)
execSubprogram = undefined

-- | Obtain list of actual parameters types.
typeArgs as m pm ss = map snd (processSubArgs as m pm ss)

-- | Process list of actual parameters from a subprogram call.
processSubArgs :: [SubprogArg] -> Memory -> ProgramMemory -> Scopes -> [(Either (Identifier, Either CellIdentifier (Maybe Value)) Value, GType)]
processSubArgs [] _ _ _ = []
processSubArgs (a:as) m pm ss = case a of
                                        ArgIdentAssign (IdentAssign [i] expr) -> (Left (i, Right (Just ev)), getType ev) : remaining
                                                                                where ev = eval m pm ss expr
                                        ArgExpr expr -> case expr of 
                                                            ArithTerm (IdTerm id@(Ident i)) -> (Left (id, Left ci), t): remaining
                                                                where 
                                                                      (ci, (t,tc)) = case fetchVar m i ss of
                                                                                        Left err -> error err
                                                                                        Right cell -> cell
                                                            _ -> (Right ev, getType ev) : remaining
                                                                 where ev = eval m pm ss expr
                                        where remaining = processSubArgs as m pm ss

execAttrStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO Memory
execAttrStmt (AttrStmt (t:ts) (v:vs)) m pm ss = case t of
                                                (ArithTerm (IdTerm (Ident i))) -> do case updateVar m i ss (makeCompatibleAssignTypes t' k) of
                                                                                            Right m' -> return m'
                                                                                            Left i -> error i
                                                                                        where k = eval m pm ss v
                                                                                              t' = getType k
                                                (ListAccess id@(ArithTerm (IdTerm (Ident i))) index) -> do case updateVar m i ss (makeCompatibleAssignTypes t' k) of
                                                                                                            Right m' -> return m'
                                                                                                            Left i -> error i 
                                                                                        where k = case (eval m pm ss id) of
                                                                                                        (List xs) -> List (setElemList xs index' (eval m pm ss v))
                                                                                                        _ -> error "You must access a list."
                                                                                              t' = getType k
                                                                                              index' = case (eval m pm ss index) of
                                                                                                        (Integer int) -> int
                                                                                                        _ -> error "List index must be an integer."
                                                (DictAccess id@(ArithTerm (IdTerm (Ident i))) index) -> do case updateVar m i ss (makeCompatibleAssignTypes t' k) of
                                                                                                            Right m' -> return m'
                                                                                                            Left i -> error i
                                                                                        where   k = case (eval m pm ss id) of
                                                                                                        (Map m') -> Map (M.insert index' (eval m pm ss v) m')
                                                                                                        _ -> error "You must access a dictionary." 
                                                                                                t' = getType k
                                                                                                index' = case getType (eval m pm ss index) of
                                                                                                            kt -> (eval m pm ss index)
                                                                                                            _ -> error "Incompatible index type."
                                                                                                (GDict kt _) = getType (eval m pm ss id)
    
                                                                                                        

-- |From value, guaarantee boolean value
makeBooleanFromValue :: Value -> Either String Bool
makeBooleanFromValue (Bool v) = Right v
makeBooleanFromValue _        = Left "Expected boolean value"

-- | Given type t and value v, return (t,v) if they are compatible.
makeCompatibleAssignTypes :: GType -> Value -> (GType, MemoryValue)
makeCompatibleAssignTypes t@(GList _) v@(List []) = (t, Value v)
makeCompatibleAssignTypes t v = if t' == t 
                                    then (t, Value v) 
                                    else error ("Incompatible types " ++ show t' ++ " and " ++ show t)
                                where
                                    t' = getType v

-- | Set the i-element of a list.
setElemList :: Integral i => [a] -> i -> a -> [a]
setElemList [] i k 
    | i >= 0 = error "Index out of range"
    | otherwise = []
setElemList (x:xs) 0 k = k : xs
setElemList (x:xs) i k = x : setElemList xs (i-1) k

-- |Executes a declaration statement.
varDeclStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO Memory
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t [])) m pm ss =     do 
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes t (defaultValue t)) m of
                                                                        (Left i) -> error i
                                                                        (Right m') -> varDeclStmt (DeclStmt (VarDeclaration xs t [])) m' pm ss 
varDeclStmt (DeclStmt (VarDeclaration [] t [])) m pm ss =         do 
                                                                return m
varDeclStmt (DeclStmt (VarDeclaration [] t (_:es))) m pm ss =     do 
                                                                error "Too many expressions in right side."
varDeclStmt (DeclStmt (VarDeclaration (x:xs'@(y:xs)) t (e:[]))) m pm ss = do 
                                                                do
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes t (eval m pm ss e)) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs' t (e:[]))) i pm ss
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t (e:es))) m pm ss = do 
                                                                do
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes t (eval m pm ss e)) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs t es)) i pm ss


{- ParamDeclaration [Identifier] GParamType [ArithExpr]
 - GParamType = GType GType | GRef GRef
 -
 -}

interpretSubDeclaration :: Subprogram -> Memory -> ProgramMemory -> Scopes -> (SubIdentifier, SubContent)
interpretSubDeclaration (Subprogram (Ident i) ps t b) m pm ss = ((i, paramTypes ps), (formalParams ps, t, b)) 
    where   
            paramTypes [] = []
            paramTypes ((ParamDeclaration _ gt _):ps) = gt : paramTypes ps

            formalParams :: [ParamDeclaration] -> [(String, GParamType, Maybe Value)]
            formalParams [] = []
            formalParams (p:ps) = (interpretParamDeclaration p m pm ss) ++ formalParams ps

interpretParamDeclaration :: ParamDeclaration -> Memory -> ProgramMemory -> Scopes -> [(String, GParamType, Maybe Value)]
interpretParamDeclaration pd@(ParamDeclaration is _ es) m pm ss 
    | length es > length is = error "Too many default values"
    | otherwise = interpretParamDeclaration' pd m pm ss
    where
        interpretParamDeclaration' (ParamDeclaration [] _ []) _ _ _ = []
        interpretParamDeclaration' (ParamDeclaration [(Ident i)] gt [e]) m pm ss = [(i, gt, Just (eval m pm ss e))]
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt []) m pm ss = (i, gt, Nothing) : interpretParamDeclaration (ParamDeclaration is gt []) m pm ss
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt [e]) m pm ss = (i, gt, Just (eval m pm ss e)) : interpretParamDeclaration (ParamDeclaration is gt [e]) m pm ss
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt (e:es)) m pm ss = (i, gt, Just (eval m pm ss e)) : interpretParamDeclaration (ParamDeclaration is gt es) m pm ss
                


{--
interpretFormalDeclarationList :: [ParamDeclaration] -> Scope -> Memory -> Scopes -> [((Name,Scope),(GType, MemoryValue))]
interpretFormalDeclarationList :: [ParamDeclaration] -> Scope -> Memory -> Scopes -> [((Name,Scope),(GType, MemoryValue))]
interpretFormalDeclarationList [] s m ss = []
interpretFormalDeclarationList (v:vs) s m ss = interpretFormalDeclaration v s m ss : (interpretFormalDeclarationList vs s m ss)


interpretFormalDeclaration :: ParamDeclaration -> Scope -> Memory -> Scopes -> [((Name, Scope), (GType, MemoryValue))]
interpretFormalDeclaration (ParamDeclaration [] t []) s m ss = []
interpretFormalDeclaration (ParamDeclaration [] t (_:vs)) s m ss = error "Too many expressions in the right side of the declaration"
interpretFormalDeclaration (ParamDeclaration ((Ident x):xs) t (v:[])) s m ss = ((x,s), makeCompatibleAssignTypes t (eval m ss v)) : (interpretFormalDeclaration (ParamDeclaration xs t [v]) s m ss)
interpretFormalDeclaration (ParamDeclaration ((Ident x):xs) t (v:vs)) s m ss = ((x,s), makeCompatibleAssignTypes t (eval m ss v)) : (interpretFormalDeclaration (ParamDeclaration xs t vs) s m ss)
--}

getType :: Value -> GType
getType (Integer i)                   = GInteger
getType (Float f)                     = GFloat
getType (String s )                   = GString
getType (Char c)                      = GChar
getType (Bool b)                      = GBool
getType (List (x:_))                  = GList (getType x)
getType (List [])                     = GList GEmpty
getType (Map (m))                     = GDict ( getType (head (M.keys m))) ( getType (head (M.elems m)))
getType (Pair (v1,v2))                = GPair (getType v1) (getType v2 )
getType (Triple (v1,v2, v3))         = GTriple (getType v1) (getType v2 ) (getType v3)
getType (Quadruple (v1,v2, v3, v4))  = GQuadruple (getType v1) (getType v2 ) (getType v3) (getType v4)

getKeyType :: Value -> GType
getKeyType (Map m)            = getType (head (M.keys m))

getValueType :: Value -> GType 
getValueType (Map m)          = getType (head (M.elems m))

evalList :: Memory -> ProgramMemory -> Scopes -> [ArithExpr] -> [Value]
evalList m pm ss [x]      =  [eval m pm ss x]
evalList m pm ss (x:y:xs) =  if getType z /= getType (eval m pm ss y) then error "Type mismatch in List "
                     else  (z:(evalList m pm ss (y:xs))) 
                     where z = (eval m pm ss x)


evalTuple :: Memory -> ProgramMemory -> Scopes -> [ArithExpr] -> [Value]
evalTuple m pm ss [x] = [eval m pm ss x]
evalTuple m pm ss (x:xs) = (z:(evalTuple m pm ss xs))
                         where z = eval m pm ss x

evalDict :: Memory -> ProgramMemory -> Scopes -> [DictEntry] -> M.Map Value Value -> M.Map Value Value
evalDict m pm ss [] m1                   = M.empty
evalDict m pm ss ((k1,v1):(k2,v2):xs) m1 = if (getType ek1) == (getType ek2) && (getType ev1 == getType ev2)
                                         then M.insert ek1 ev1 (evalDict m pm ss ((k2,v2):xs) m1) 
                                        else error "Dict Type mismatch"
                                         where ek1 = eval m pm ss k1
                                               ek2 = eval m pm ss k2
                                               ev1 = eval m pm ss v1
                                               ev2 = eval m pm ss v2
evalDict m pm ss ((k,v):xs) m1           = M.insert (eval m pm ss k) (eval m pm ss v) (evalDict m pm ss xs m1)

-- | Default values for each type
defaultValue :: GType -> Value
defaultValue GInteger = Integer 0
defaultValue GFloat = Float 0.0
defaultValue GString = String []
defaultValue GBool = Bool False
defaultValue (GList _) = List []
defaultValue (GPair t1 t2) = Pair (defaultValue t1, defaultValue t2)
defaultValue (GTriple t1 t2 t3) = Triple (defaultValue t1, defaultValue t2, defaultValue t3)
defaultValue (GQuadruple t1 t2 t3 t4) = Quadruple (defaultValue t1, defaultValue t2, defaultValue t3, defaultValue t4)
defaultValue (GDict k v) = Map (M.empty)


fromValue :: Value -> Integer
fromValue (Integer i) = i  

evalBinOp ::Memory -> ProgramMemory -> Scopes -> ArithExpr ->( Value -> Value -> Value )-> Value
evalBinOp m pm ss (ArithBinExpr _  e1 e2) f = case eval m pm ss e1 of
                                                     l1@(List (x:xs)) -> if (getType k == getType x) then f l1 k
                                                                         else error "Type mismatch operation"
                                                                          where k = eval m pm ss e2 
                                                     k -> case eval m pm ss e2 of 
                                                           l2@(List (x:xs) ) -> if (getType k == getType x) then f l2 k 
                                                                         else error "Type mismatch  operation"
                                                           x -> if (getType k == getType x) then f k  x
                                                                         else error "Type mismatch  operation"

eval :: Memory -> ProgramMemory -> Scopes -> ArithExpr -> Value
eval m pm ss (ArithTerm (LitTerm (Lit v)))      = v
eval m pm ss (ArithUnExpr MinusUnOp e)          = minusUn (eval m pm ss e)
eval m pm ss (ArithUnExpr PlusUnOp e)           = plusUn (eval m pm ss e)
eval m pm ss (ArithUnExpr NotUnOp e)            = not' (eval m pm ss e)
eval m pm ss (ArithBinExpr MinusBinOp  e1 e2)   = evalBinOp m pm ss (ArithBinExpr MinusBinOp  e1 e2) minusBin  
eval m pm ss (ArithBinExpr PlusBinOp  e1 e2)    = evalBinOp m pm ss (ArithBinExpr PlusBinOp  e1 e2) plusBin
eval m pm ss (ArithBinExpr TimesBinOp  e1 e2)   = evalBinOp m pm ss (ArithBinExpr TimesBinOp e1 e2) timesBin
eval m pm ss (ArithBinExpr DivBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr DivBinOp e1 e2) divBin
eval m pm ss (ArithBinExpr ExpBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr ExpBinOp e1 e2) expBin  
eval m pm ss (ArithBinExpr ModBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr ModBinOp e1 e2) modBin  
eval m pm ss (ExprLiteral (ListLit [] ))        = List []
eval m pm ss (ExprLiteral (ListLit es ))        = List (evalList m pm ss es)
eval m pm ss (ExprLiteral (DictLit de))         = Map (evalDict m pm ss de M.empty )
eval m pm ss (ArithEqExpr Equals e1 e2)         = Bool (eval m pm ss e1 == eval m pm ss e2)
eval m pm ss (ArithEqExpr NotEquals e1 e2)      = Bool (eval m pm ss e1 /= eval m pm ss e2)
eval m pm ss (ExprLiteral (TupleLit te))        = if length l == 2 then Pair ((l !! 0), (l !! 1))
                                               else if length l == 3 then Triple ((l !! 0), (l !! 1), (l !! 2))
                                                    else if length  l == 4 then Quadruple ((l !! 0), (l !! 1), (l !! 2), (l !! 3))
                                                     else error "Limit of Quadruples"
                                                       where l = evalTuple m pm ss te
eval m pm ss (ListAccess e1 e2 )                = case eval m pm ss e1 of
                                                (List l) -> case eval m pm ss e2 of
                                                             Integer i ->  l !! (fromIntegral i)
                                                             _ -> error "Access List mismatch"
                                                _ -> error "Access List mismatch"
eval m pm ss (DictAccess e1 e2)                 = case eval m pm ss e1 of
                                                d@(Map ma) -> if getKeyType d == getType k then case  M.lookup k ma of
                                                                                                      Nothing -> error "No key on Dict "
                                                                                                      Just k  -> k
                                                             else error "Access Dict with invalid key type"
                                                              where k = eval m pm ss e2 
                                                _ -> error "Access on Dict type mismatch"
eval m pm ss (TupleAccess e1 e2)                = case eval m pm ss e1 of
                                                (Pair (v1, v2)) -> case eval m pm ss e2 of
                                                                    Integer 0 -> v1
                                                                    Integer 1 -> v2
                                                                    _         -> error "Acessing Pair"
                                                Triple (v1,v2,v3)-> case eval m pm ss e2 of
                                                                    Integer 0 -> v1
                                                                    Integer 1 -> v2
                                                                    Integer 2 -> v3
                                                                    _         -> error "Acessing Pair"
                                                Quadruple (v1,v2,v3,v4)-> case eval m pm ss e2 of
                                                                    Integer 0 -> v1
                                                                    Integer 1 -> v2
                                                                    Integer 2 -> v3
                                                                    Integer 3 -> v4
                                                                    _         -> error "Acessing Pair"
                                                 
                                                _ -> error "Tuple error " 

eval m pm ss (ArithBinExpr PlusPlusBinOp e1 e2) = case eval m pm ss e1 of
                                                l1@(List (x:xs)) -> case eval m pm ss e2 of
                                                                        l2@(List (y:ys)) -> if (getType x == getType y) then plusPlusBinList l1 l2
                                                                                            else plusPlusBin l1 l2
                                                                        k -> if (getType k == getType x) then plusPlusBin l1 k
                                                                             else error "Type mismatch ++ opeator "
                                                k -> case eval m pm ss e2 of 
                                                        l2@(List (y:ys)) -> if (getType k == getType y) then plusPlusBin k l2
                                                                            else error "Type mismatch ++ operator "
eval m pm ss (ArithTerm (IdTerm (Ident i))) = case fetchVarValue m i ss of
                                                Left i -> error i
                                                Right i -> i


plusPlusBinList :: Value -> Value -> Value
plusPlusBinList (List xs'@(x:xs)) (List ys'@(y:ys)) = List (xs' ++ ys')

plusPlusBin :: Value -> Value -> Value
plusPlusBin k (List xs'@(x:xs)) = if getType (k) /= tl then error "Type mismatch in operation ++"
                           else List (k:xs')
                                where tl = getType x 
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
plusBin (Integer i) (Integer j) = (Integer (i+j)) 
plusBin (Float f) (Integer i )  = ( Float (f + (fromInteger i)))
plusBin (Integer i) ( Float f)  = ( Float ((fromInteger i) +f))  
plusBin (Float f1) (Float f2)   = ( Float (f1 + f2))
plusBin (List l)  i             =   List ( map (plusBin i) l)
plusBin  i (List l)             =   List ( map (plusBin i) l)

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
