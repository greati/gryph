module Execution.Semantic where

import Syntactic.Values
import Syntactic.Syntax
import Execution.Memory
import Syntactic.Parser 
import qualified Data.Map as M


type Filename = String

-- |Scopes in the execution.
scopes :: Scopes
scopes = ["global"]

-- |Main interpreter function.
igryph :: Filename -> IO()
igryph s = do
                us <- parseFile s
                exec memory scopes us

-- |Execute a program represented as a list of program units.
exec :: Memory -> Scopes -> [ProgramUnit] -> IO() 
exec m ss [] = return ()
exec m ss (u:us) = do
                        (m', ss') <- execUnit u m ss
                        print (show m')
                        exec m' ss' us 

-- |Executes a program unit.
execUnit :: ProgramUnit -> Memory -> Scopes -> IO (Memory, Scopes)
execUnit (Subprogram sub) m ss = do 
                                    execSubDecl sub m
                                    return (m,ss)
execUnit (StructDecl struct) m ss = 
                                do
                                    execStructDecl struct m
                                    return (m,ss)
execUnit (Stmt stmt) m ss = execStmt stmt m ss

execSubDecl :: Subprogram -> Memory -> IO ()
execSubDecl s m = undefined

execStructDecl :: StructDecl -> Memory -> IO ()
execStructDecl s m = undefined

-- |Executes any statement.
execStmt :: Stmt -> Memory -> Scopes -> IO (Memory, Scopes)
execStmt d@(DeclStmt _) m ss = do
                            m' <- varDeclStmt d m ss
                            return (m', ss)
execStmt a@(AttrStmt _ _) m ss = do
                            m' <- execAttrStmt a m ss
                            return (m', ss)
execStmt (PrintStmt e) m ss = do
                                putStrLn (show (eval m ss e))
                                return (m, ss)
                            
execAttrStmt :: Stmt -> Memory -> Scopes -> IO Memory
execAttrStmt (AttrStmt (t:ts) (v:vs)) m ss = case t of
                                                (ArithTerm (IdTerm (Ident i))) -> do case updateVar m i ss (makeCompatibleAssignTypes t' k) of
                                                                                            Right m' -> return m'
                                                                                            Left i -> error i
                                                                                        where k = eval m ss v
                                                                                              t' = getType k
                                                (ListAccess id@(ArithTerm (IdTerm (Ident i))) index) -> do case updateVar m i ss (makeCompatibleAssignTypes t' k) of
                                                                                                            Right m' -> return m'
                                                                                                            Left i -> error i 
                                                                                        where k = case (eval m ss id) of
                                                                                                        (List xs) -> List (setElemList xs index' (eval m ss v))
                                                                                                        _ -> error "You must access a list."
                                                                                              t' = getType k
                                                                                              index' = case (eval m ss index) of
                                                                                                        (Integer int) -> int
                                                                                                        _ -> error "List index must be an integer."
                                                (DictAccess id@(ArithTerm (IdTerm (Ident i))) index) -> do case updateVar m i ss (makeCompatibleAssignTypes t' k) of
                                                                                                            Right m' -> return m'
                                                                                                            Left i -> error i
                                                                                        where   k = case (eval m ss id) of
                                                                                                        (Map m') -> Map (M.insert index' (eval m ss v) m')
                                                                                                        _ -> error "You must access a dictionary." 
                                                                                                t' = getType k
                                                                                                index' = case getType (eval m ss index) of
                                                                                                            kt -> (eval m ss index)
                                                                                                            _ -> error "Incompatible index type."
                                                                                                (GDict kt _) = getType (eval m ss id)
    
                                                                                                        
-- | Given type t and value v, return (t,v) if they are compatible.
makeCompatibleAssignTypes :: GType -> Value -> (GType, Value)
makeCompatibleAssignTypes t@(GList _) v@(List []) = (t,v)
makeCompatibleAssignTypes t v = if t' == t 
                                    then (t,v) 
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
varDeclStmt :: Stmt -> Memory -> Scopes -> IO Memory
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t [])) m ss =     do 
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes t (defaultValue t)) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs t [])) i ss
varDeclStmt (DeclStmt (VarDeclaration [] t [])) m ss =         do 
                                                                return m
varDeclStmt (DeclStmt (VarDeclaration [] t (_:es))) m ss =     do 
                                                                error "Too many expressions in right side."
varDeclStmt (DeclStmt (VarDeclaration (x:xs'@(y:xs)) t (e:[]))) m ss = do 
                                                                do
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes t (eval m ss e)) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs' t (e:[]))) i ss
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t (e:es))) m ss = do 
                                                                do
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes t (eval m ss e)) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs t es)) i ss


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

evalList :: Memory -> Scopes -> [ArithExpr] -> [Value]
evalList m ss [x]      =  [eval m ss x]
evalList m ss (x:y:xs) =  if getType z /= getType (eval m ss y) then error "Type mismatch in List "
                     else  (z:(evalList m ss (y:xs))) 
                     where z = (eval m ss x)


evalTuple :: Memory -> Scopes -> [ArithExpr] -> [Value]
evalTuple m ss [x] = [eval m ss x]
evalTuple m ss (x:xs) = (z:(evalTuple m ss xs))
                         where z = eval m ss x
evalDict :: Memory -> Scopes -> [DictEntry] -> M.Map Value Value -> M.Map Value Value
evalDict m ss [] m1                   = M.empty
evalDict m ss ((k1,v1):(k2,v2):xs) m1 = if (getType ek1) == (getType ek2) && (getType ev1 == getType ev2)
                                         then M.insert ek1 ev1 (evalDict m ss ((k2,v2):xs) m1) 
                                        else error "Dict Type mismatch"
                                         where ek1 = eval m ss k1
                                               ek2 = eval m ss k2
                                               ev1 = eval m ss v1
                                               ev2 = eval m ss v2
evalDict m ss ((k,v):xs) m1           = M.insert (eval m ss k) (eval m ss v) (evalDict m ss xs m1)

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

evalBinOp ::Memory -> Scopes -> ArithExpr ->( Value -> Value -> Value )-> Value
evalBinOp m ss (ArithBinExpr _  e1 e2) f = case eval m ss e1 of
                                                     l1@(List (x:xs)) -> if (getType k == getType x) then f l1 k
                                                                         else error "Type mismatch operation"
                                                                          where k = eval m ss e2 
                                                     k -> case eval m ss e2 of 
                                                           l2@(List (x:xs) ) -> if (getType k == getType x) then f l2 k 
                                                                         else error "Type mismatch  operation"
                                                           x -> if (getType k == getType x) then f k  x
                                                                         else error "Type mismatch  operation"

eval :: Memory -> Scopes -> ArithExpr -> Value
eval m ss (ArithTerm (LitTerm (Lit v)))      = v
eval m ss (ArithUnExpr MinusUnOp e)          = minusUn (eval m ss e)
eval m ss (ArithUnExpr PlusUnOp e)           = plusUn (eval m ss e)
eval m ss (ArithUnExpr NotUnOp e)            = not' (eval m ss e)
eval m ss (ArithBinExpr MinusBinOp  e1 e2)   = evalBinOp m ss (ArithBinExpr MinusBinOp  e1 e2) minusBin  
eval m ss (ArithBinExpr PlusBinOp  e1 e2)    = evalBinOp m ss (ArithBinExpr PlusBinOp  e1 e2) plusBin
eval m ss (ArithBinExpr TimesBinOp  e1 e2)   = evalBinOp m ss (ArithBinExpr TimesBinOp e1 e2) timesBin
eval m ss (ArithBinExpr DivBinOp  e1 e2)     = evalBinOp m ss (ArithBinExpr DivBinOp e1 e2) divBin
eval m ss (ArithBinExpr ExpBinOp  e1 e2)     = evalBinOp m ss (ArithBinExpr ExpBinOp e1 e2) expBin  
eval m ss (ArithBinExpr ModBinOp  e1 e2)     = evalBinOp m ss (ArithBinExpr ModBinOp e1 e2) modBin  
eval m ss (ExprLiteral (ListLit [] ))        = List []
eval m ss (ExprLiteral (ListLit es ))        = List (evalList m ss es)
eval m ss (ExprLiteral (DictLit de))         = Map (evalDict m ss de M.empty )
eval m ss (ExprLiteral (TupleLit te))        = if length l == 2 then Pair ((l !! 0), (l !! 1))
                                               else if length l == 3 then Triple ((l !! 0), (l !! 1), (l !! 2))
                                                    else if length  l == 4 then Quadruple ((l !! 0), (l !! 1), (l !! 2), (l !! 3))
                                                     else error "Limit of Quadruples"
                                                       where l = evalTuple m ss te
eval m ss (ListAccess e1 e2 )                = case eval m ss e1 of
                                                (List l) -> case eval m ss e2 of
                                                             Integer i ->  l !! (fromIntegral i)
                                                             _ -> error "Access List mismatch"
                                                _ -> error "Access List mismatch"
eval m ss (DictAccess e1 e2)                 = case eval m ss e1 of
                                                d@(Map ma) -> if getKeyType d == getType k then case  M.lookup k ma of
                                                                                                      Nothing -> error "No key on Dict "
                                                                                                      Just k  -> k
                                                             else error "Access Dict with invalid key type"
                                                              where k = eval m ss e2 
                                                _ -> error "Access on Dict type mismatch"
eval m ss (TupleAccess e1 e2)                = case eval m ss e1 of
                                                (Pair (v1, v2)) -> case eval m ss e2 of
                                                                    Integer 0 -> v1
                                                                    Integer 1 -> v2
                                                                    _         -> error "Acessing Pair"
                                                Triple (v1,v2,v3)-> case eval m ss e2 of
                                                                    Integer 0 -> v1
                                                                    Integer 1 -> v2
                                                                    Integer 2 -> v3
                                                                    _         -> error "Acessing Pair"
                                                Quadruple (v1,v2,v3,v4)-> case eval m ss e2 of
                                                                    Integer 0 -> v1
                                                                    Integer 1 -> v2
                                                                    Integer 2 -> v3
                                                                    Integer 3 -> v4
                                                                    _         -> error "Acessing Pair"
                                                 
                                                _ -> error "Tuple error " 

eval m ss (ArithBinExpr PlusPlusBinOp e1 e2) = case eval m ss e1 of
                                                l1@(List (x:xs)) -> case eval m ss e2 of
                                                                        l2@(List (y:ys)) -> if (getType x == getType y) then plusPlusBinList l1 l2
                                                                                            else plusPlusBin l1 l2
                                                                        k -> if (getType k == getType x) then plusPlusBin l1 k
                                                                             else error "Type mismatch ++ opeator "
                                                k -> case eval m ss e2 of 
                                                        l2@(List (y:ys)) -> if (getType k == getType y) then plusPlusBin k l2
                                                                            else error "Type mismatch ++ operator "
eval m ss (ArithTerm (IdTerm (Ident i))) = case fetchVarValue m i ss of
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
