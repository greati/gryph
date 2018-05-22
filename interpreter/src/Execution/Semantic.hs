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

eval :: ArithExpr -> Value
eval (ArithTerm (LitTerm (Lit v))) = v
eval (ArithUnExpr MinusUnOp e) = minusUn (eval e)
eval (ArithUnExpr PlusUnOp e) = plusUn (eval e)
eval (ArithUnExpr NotUnOp e) = not' (eval e)
eval (ArithBinExpr MinusBinOp  e1 e2) = minusBin (eval e1) (eval e2)  



minusBin ::  Value -> Value -> Value
minusBin (Integer i) (Integer j) = (Integer (i-j)) 
minusBin (Float f) (Integer i )  = ( Float (f - (fromInteger i)))
minusBin (Integer i) ( Float f)  = ( Float ((fromInteger i) -f))  
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
