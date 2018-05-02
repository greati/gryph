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
stmt = try(readStmt)
    <|> try(printStmt)
    <|> try(attrStmt)
    <|> try(declStmt)

attrStmt :: GenParser GphTokenPos st Stmt
attrStmt = do
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
declStmt = do
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
                e <- arithExpr
                (tok GTokPlus)
                t <- termArithExpr
                return (ArithBinExpr PlusBinOp e t)
            <|>
            do
                e <- arithExpr
                (tok GTokMinus)
                t <- termArithExpr
                return (ArithBinExpr MinusBinOp e t)
            <|>
            do
                termArithExpr
                

termArithExpr :: GenParser GphTokenPos st ArithExpr
termArithExpr = do
                    (tok GTokMinus)
                    t <- termArithExpr
                    return (ArithUnExpr MinusUnOp t)
                <|>
                do
                    t <- termArithExpr
                    (tok GTokModulus)
                    f <- factorArithExpr
                    return (ArithBinExpr PlusBinOp t f)
                <|>
                do
                    t <- termArithExpr
                    (tok GTokTimes)
                    f <- factorArithExpr
                    return (ArithBinExpr TimesBinOp t f)
                <|>
                do
                    t <- termArithExpr
                    (tok GTokDivision)
                    f <- factorArithExpr
                    return (ArithBinExpr DivBinOp t f)
                <|>
                do
                    t <- termArithExpr
                    (tok GTokHat)
                    f <- factorArithExpr
                    return (ArithBinExpr ExpBinOp t f)
                <|>
                do
                    factorArithExpr
                    

factorArithExpr :: GenParser GphTokenPos st ArithExpr
factorArithExpr = do
                    (tok GTokLParen)
                    e <- arithExpr
                    (tok GTokRParen)
                    return e 
                  <|> 
                  do 
                    n <- (numberLit)
                    return (ArithTerm n)
                  <|>
                  do 
                    i <- anyIdent
                    return (ArithTerm i)


parseFile :: String -> IO [Stmt]
parseFile file = 
    do program <- readFile file
       case parse gryphParser "" (alexScanTokens program) of
            Left e  -> print e >> fail "parse error"
            Right r -> return r


