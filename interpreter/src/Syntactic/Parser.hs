module Parsing.Parser where

import Lexer
import Text.ParserCombinators.Parsec
import GTok
import GphTokens

type Identifier = String
type Type = String
data Stmt = ReadStmt Identifier | PrintStmt Identifier | DeclStmt Identifier Type | AttrStmt Identifier Value deriving (Show, Eq)
data Value = AritValue ArithExpr deriving (Show, Eq)

gryphParser :: GenParser GphTokenPos st [Stmt]
gryphParser = 
    do result <- many stmt
       return result

stmt :: GenParser GphTokenPos st Stmt
stmt = readStmt
    <|> printStmt
    <|> startIdentStmt

startIdentStmt :: GenParser GphTokenPos st Stmt
startIdentStmt = do
                    i <- anyIdent
                    do
                        attrStmt i <|> declStmt i

attrStmt :: Identifier -> GenParser GphTokenPos st Stmt
attrStmt i = do 
                (tok GTokAssignment)
                v <- arithExpr
                (tok GTokSemicolon)
                return (AttrStmt i (AritValue v))

declStmt :: Identifier -> GenParser GphTokenPos st Stmt
declStmt i = do
                (tok GTokColon)
                t <- anyType
                (tok GTokSemicolon)
                return (DeclStmt i t)

readStmt :: GenParser GphTokenPos st Stmt
readStmt = do 
                (tok GTokRead) 
                i <- anyIdent
                (tok GTokSemicolon)
                return (ReadStmt i) 

printStmt :: GenParser GphTokenPos st Stmt
printStmt = do
                (tok GTokPrint) 
                i <- anyIdent
                (tok GTokSemicolon)
                return (PrintStmt i) 


data ArithUnOp = MinusUnOp | PlusUnOp deriving (Show, Eq)
data ArithBinOp = MinusBinOp | PlusBinOp | TimesBinOp | DivBinOp | ModBinOp | ExpBinOp deriving (Show, Eq)
data ArithExpr = ArithUnExpr ArithUnOp ArithExpr | ArithBinExpr ArithBinOp ArithExpr ArithExpr | ArithTerm String deriving (Show, Eq)


arithExpr :: GenParser GphTokenPos st ArithExpr
arithExpr = do
                t <- termArithExpr
                do
                    arithExprAux t <|> return t

opUnary :: GenParser GphTokenPos st ArithUnOp
opUnary = do (tok GTokPlus) >> return PlusUnOp
        <|>
        do (tok GTokMinus) >> return MinusUnOp

opZero :: GenParser GphTokenPos st ArithBinOp
opZero = do (tok GTokPlus) >> return PlusBinOp 
        <|>
        do (tok GTokMinus) >> return MinusBinOp

arithExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
arithExprAux t = do 
                    op <- opZero
                    f <- termArithExpr
                    do 
                        arithExprAux (ArithBinExpr op t f)
                        <|> return (ArithBinExpr op t f)

termArithExpr :: GenParser GphTokenPos st ArithExpr
termArithExpr = do 
                    op <- opUnary
                    t <- termArithExpr
                    do
                        termArithExprAux t
                        <|> return (ArithUnExpr op t)
                <|>
                do
                    f <- factorArithExpr
                    do
                        termArithExprAux f
                        <|> return f
 
opOne :: GenParser GphTokenPos st ArithBinOp
opOne = do (tok GTokModulus) >> return ModBinOp 
        <|> 
        do (tok GTokDivision) >> return DivBinOp 
        <|> 
        do (tok GTokTimes) >> return TimesBinOp
        <|>
        do (tok GTokHat) >> return ExpBinOp
            

termArithExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
termArithExprAux t = do 
                        op <- opOne
                        f <- factorArithExpr
                        do
                            termArithExprAux (ArithBinExpr op t f)
                            <|> return (ArithBinExpr op t f)
 
factorArithExpr :: GenParser GphTokenPos st ArithExpr
factorArithExpr = do 
                    n <- (numberLit)
                    return (ArithTerm n)
                  <|>
                  do 
                    i <- anyIdent
                    return (ArithTerm i)
                  <|>
                  do
                    (tok GTokLParen)
                    e <- arithExpr
                    (tok GTokRParen)
                    return e 

parseFile :: String -> IO [Stmt]
parseFile file = 
    do program <- readFile file
       case parse gryphParser "" (alexScanTokens program) of
            Left e  -> print e >> fail "parse error"
            Right r -> return r


