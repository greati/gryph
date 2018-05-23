module Execution.Semantic where

import Syntactic.Values
import Syntactic.Syntax
import Execution.Memory
import Syntactic.Parser 


type Filename = String

scopes = []

gryph :: Filename -> IO()
--gryph s = exec memory scopes (parseFile s) 
gryph = undefined

exec :: Memory -> [Scope] -> [ProgramUnit] -> IO() 
exec = undefined

execStmt :: Stmt -> IO()
execStmt (DeclStmt (VarDeclaration [x] t [e])) = undefined

getType :: Value -> GType
getType (Integer i)      = GInteger
getType (Float f)        = GFloat
getType (String s )      = GString
getType (Char c)         = GChar
getType (Bool b)         = GBool
getType (List (x:_))     = GList (getType x)

evalList :: [ArithExpr] -> [Value]
evalList [x]      =  [eval x]
evalList (x:y:xs) =  if getType z /= getType (eval y) then error "Type mismatch in List "
                     else  z:(evalList (y:xs)) 
		     where z = (eval x)

fromValue :: Value -> Integer
fromValue (Integer i) = i  


eval :: ArithExpr -> Value
eval (ArithTerm (LitTerm (Lit v)))     = v
eval (ArithUnExpr MinusUnOp e)         = minusUn (eval e)
eval (ArithUnExpr PlusUnOp e)          = plusUn (eval e)
eval (ArithUnExpr NotUnOp e)           = not' (eval e)
eval (ArithBinExpr MinusBinOp  e1 e2)  = minusBin (eval e1) (eval e2)  
eval (ArithBinExpr PlusBinOp  e1 e2)   = plusBin (eval e1) (eval e2)  
eval (ArithBinExpr TimesBinOp  e1 e2)  = timesBin (eval e1) (eval e2)  
eval (ArithBinExpr DivBinOp  e1 e2)    = divBin (eval e1) (eval e2)  
eval (ArithBinExpr ExpBinOp  e1 e2)    = expBin (eval e1) (eval e2)  
eval (ExprLiteral (ListLit es ))       = List (evalList es)
eval (ArithBinExpr PlusPlusBinOp e1 e2) = case eval e1 of
						l1@(List (x:xs)) -> case eval e2 of
									l2@(List (y:ys)) -> if (getType x == getType y) then plusPlusBinList l1 l2
                                                      					    else plusPlusBin l1 l2
						k -> case eval e2 of 
							l2@(List (y:ys)) -> if (getType k == getType y) then plusPlusBin k l2
									    else error "Type mismatch ++ operator "


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
