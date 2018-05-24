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
execStmt (PrintStmt e) m ss = do
                                putStrLn (show (eval m ss e))
                                return (m, ss)
                            

-- |Executes a declaration statement.
varDeclStmt :: Stmt -> Memory -> Scopes -> IO Memory
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t [])) m ss =     do 
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (t, ([defaultValue t])) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs t [])) i ss
varDeclStmt (DeclStmt (VarDeclaration [] t [])) m ss =         do 
                                                                return m
varDeclStmt (DeclStmt (VarDeclaration [] t (_:es))) m ss =     do 
                                                                error "Too many expressions in right side."
varDeclStmt (DeclStmt (VarDeclaration (x:xs'@(y:xs)) t (e:[]))) m ss = do 
                                                                do
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (t, ([eval m ss e])) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs' t (e:[]))) i ss
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t (e:es))) m ss = do 
                                                                do
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (t, ([eval m ss e])) m of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs t es)) i ss

getType :: Value -> GType
getType (Integer i)      = GInteger
getType (Float f)        = GFloat
getType (String s )      = GString
getType (Char c)         = GChar
getType (Bool b)         = GBool
getType (List (x:_))     = GList (getType x)

evalList :: Memory -> Scopes -> [ArithExpr] -> [Value]
evalList m ss [x]      =  [eval m ss x]
evalList m ss (x:y:xs) =  if getType z /= getType (eval m ss y) then error "Type mismatch in List "
                     else  z:(evalList m ss (y:xs)) 
		     where z = (eval m ss x)


-- | Default values for each type
defaultValue :: GType -> Value
defaultValue GInteger = Integer 0
defaultValue GFloat = Float 0.0
defaultValue GString = String []
defaultValue (GList t) = List []
defaultValue (GPair t1 t2) = Pair (defaultValue t1, defaultValue t2)
defaultValue (GTriple t1 t2 t3) = Triple (defaultValue t1, defaultValue t2, defaultValue t3)
defaultValue (GQuadruple t1 t2 t3 t4) = Quadruple (defaultValue t1, defaultValue t2, defaultValue t3, defaultValue t4)
defaultValue (GDict k v) = Map (M.empty)


fromValue :: Value -> Integer
fromValue (Integer i) = i  


eval :: Memory -> Scopes -> ArithExpr -> Value
eval m ss (ArithTerm (LitTerm (Lit v)))     = v
eval m ss (ArithUnExpr MinusUnOp e)         = minusUn (eval m ss e)
eval m ss (ArithUnExpr PlusUnOp e)          = plusUn (eval m ss e)
eval m ss (ArithUnExpr NotUnOp e)           = not' (eval m ss e)
eval m ss (ArithBinExpr MinusBinOp  e1 e2)  = minusBin (eval m ss e1) (eval m ss e2)  
eval m ss (ArithBinExpr PlusBinOp  e1 e2)   = plusBin (eval m ss e1) (eval m ss e2)  
eval m ss (ArithBinExpr TimesBinOp  e1 e2)  = timesBin (eval m ss e1) (eval m ss e2)  
eval m ss (ArithBinExpr DivBinOp  e1 e2)    = divBin (eval m ss e1) (eval m ss e2)  
eval m ss (ArithBinExpr ExpBinOp  e1 e2)    = expBin (eval m ss e1) (eval m ss e2)  
eval m ss (ExprLiteral (ListLit es ))       = List (evalList m ss es)
eval m ss (ArithBinExpr PlusPlusBinOp e1 e2) = case eval m ss e1 of
						l1@(List (x:xs)) -> case eval m ss e2 of
									l2@(List (y:ys)) -> if (getType x == getType y) then plusPlusBinList l1 l2
                                                      					    else plusPlusBin l1 l2
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


modBin ::  Value -> Value -> Value
modBin (Integer i) (Integer j) = (Integer (i `mod`  j)) 

expBin ::  Value -> Value -> Value
expBin (Integer i) (Integer j) = (Integer (i ^ j)) 
expBin (Float f) (Integer i )  = ( Float (f ** (fromInteger i)))
expBin (Integer i) ( Float f)  = ( Float ((fromInteger i) ** f))  
expBin (Float f1) (Float f2)   = ( Float (f1 ** f2))

divBin ::  Value -> Value -> Value
divBin (Integer i) (Integer j) = (Integer (i `div` j)) 
divBin (Float f) (Integer i )  = ( Float (f / (fromInteger i)))
divBin (Integer i) ( Float f)  = ( Float ((fromInteger i) / f))  
divBin (Float f1) (Float f2)   = ( Float (f1 / f2))

timesBin ::  Value -> Value -> Value
timesBin (Integer i) (Integer j) = (Integer (i*j)) 
timesBin (Float f) (Integer i )  = ( Float (f * (fromInteger i)))
timesBin (Integer i) ( Float f)  = ( Float ((fromInteger i) * f))  
timesBin (Float f1) (Float f2)   = ( Float (f1 * f2))

plusBin ::  Value -> Value -> Value
plusBin (Integer i) (Integer j) = (Integer (i+j)) 
plusBin (Float f) (Integer i )  = ( Float (f + (fromInteger i)))
plusBin (Integer i) ( Float f)  = ( Float ((fromInteger i) +f))  
plusBin (Float f1) (Float f2)   = ( Float (f1 + f2))

minusBin ::  Value -> Value -> Value
minusBin (Integer i) (Integer j) = (Integer (i-j)) 
minusBin (Float f) (Integer i )  = ( Float (f - (fromInteger i)))
minusBin (Integer i) ( Float f)  = ( Float ((fromInteger i) - f))  
minusBin (Float f1) (Float f2)   = ( Float (f1 - f2))

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
