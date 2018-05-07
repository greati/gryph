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
    <|> attrStmt
    <|> declStmt

attrStmt :: GenParser GphTokenPos st Stmt
attrStmt = try $ do
                i <- anyIdent
                (tok GTokAssignment)
                v <- arithExpr
                (tok GTokSemicolon)
                return (AttrStmt i (AritValue v))

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

declStmt :: GenParser GphTokenPos st Stmt
declStmt = try $ do
                i <- anyIdent
                (tok GTokColon)
                t <- anyType
                (tok GTokSemicolon)
                return (DeclStmt i t)


data ArithUnOp = MinusUnOp deriving (Show, Eq)
data ArithBinOp = MinusBinOp | PlusBinOp | TimesBinOp | DivBinOp | ModBinOp | ExpBinOp deriving (Show, Eq)
data ArithExpr = ArithUnExpr ArithUnOp ArithExpr | ArithBinExpr ArithBinOp ArithExpr ArithExpr | ArithTerm String deriving (Show, Eq)


arithExpr :: GenParser GphTokenPos st ArithExpr
arithExpr = do
                try $ do
                    t <- termArithExpr
                    arithExprAux t
            <|>
            do 
                termArithExpr


arithExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
arithExprAux t = do
                    (tok GTokPlus)
                    f <- termArithExpr
                    return (ArithBinExpr PlusBinOp t f)
                <|>
                do
                    (tok GTokPlus)
                    f <- termArithExpr
                    arithExprAux (ArithBinExpr PlusBinOp t f)
                <|>
                do
                    (tok GTokMinus)
                    f <- termArithExpr
                    arithExprAux (ArithBinExpr MinusBinOp t f)
                <|>
                do
                    (tok GTokMinus)
                    f <- termArithExpr
                    return (ArithBinExpr MinusBinOp t f)
                    

termArithExpr :: GenParser GphTokenPos st ArithExpr
termArithExpr = do  
                    factorArithExpr
                <|>
                do
                    (tok GTokMinus)
                    t <- termArithExpr
                    return (ArithUnExpr MinusUnOp t)
                <|>
                do
                    (tok GTokMinus)
                    t <- termArithExpr
                    termArithExprAux t
                <|>
                do
                    f <- factorArithExpr
                    termArithExprAux f
 
termArithExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
termArithExprAux t = do
                        (tok GTokModulus)
                        f <- factorArithExpr
                        return (ArithBinExpr ModBinOp t f)
                    <|>
                    do
                        (tok GTokModulus)
                        f <- factorArithExpr
                        termArithExprAux (ArithBinExpr ModBinOp t f)
                    <|>
                    do
                        (tok GTokTimes)
                        f <- factorArithExpr
                        return (ArithBinExpr TimesBinOp t f)
                    <|>
                    do
                        (tok GTokTimes)
                        f <- factorArithExpr
                        termArithExprAux (ArithBinExpr TimesBinOp t f)
                    <|>
                    do
                        (tok GTokDivision)
                        f <- factorArithExpr
                        return (ArithBinExpr DivBinOp t f)
                    <|>
                    do
                        (tok GTokDivision)
                        f <- factorArithExpr
                        termArithExprAux (ArithBinExpr DivBinOp t f)
                    <|>
                    do
                        (tok GTokHat)
                        f <- factorArithExpr
                        return (ArithBinExpr ExpBinOp t f)
                    <|>
                    do
                        (tok GTokHat)
                        f <- factorArithExpr
                        termArithExprAux (ArithBinExpr ExpBinOp t f)
                        
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


