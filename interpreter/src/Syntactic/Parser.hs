module Syntactic.Parser where

import Syntactic.Lexer
import Text.ParserCombinators.Parsec
import Syntactic.GTok
import Syntactic.GphTokens
import Execution.Memory
import Syntactic.Values
import Syntactic.Syntax

gryphParser :: GenParser GphTokenPos st [Stmt]
gryphParser = 
    do result <- many stmt
       return result

stmt :: GenParser GphTokenPos st Stmt
stmt = readStmt
    <|> printStmt
    <|> startIdentListStmt

startIdentListStmt :: GenParser GphTokenPos st Stmt
startIdentListStmt = do
                    i <- identList
                    do
                        attrStmt i <|> declStmt i

attrStmt :: [Identifier] -> GenParser GphTokenPos st Stmt
attrStmt is = do 
                (tok GTokAssignment)
                vs <- arithExprList
                (tok GTokSemicolon)
                return (AttrStmt is vs)

declStmt :: [Identifier] -> GenParser GphTokenPos st Stmt
declStmt is = do
                (tok GTokColon)
                t <- anyType
                do
                    (tok GTokSemicolon)
                    return (DeclStmt is t [])
                    <|>
                    do
                        (tok GTokAssignment)
                        es <- arithExprList
                        (tok GTokSemicolon)
                        return (DeclStmt is t es)
                    

readStmt :: GenParser GphTokenPos st Stmt
readStmt = do 
                (tok GTokRead) 
                i <- anyIdent
                (tok GTokSemicolon)
                return (ReadStmt i) 

printStmt :: GenParser GphTokenPos st Stmt
printStmt = do
                (tok GTokPrint) 
                do
                    do
                        i <- anyIdent 
                        (tok GTokSemicolon)
                        return (PrintStmt (IdTerm i)) 
                    <|>
                    do
                        i <- stringLit
                        (tok GTokSemicolon)
                        return (PrintStmt (LitTerm i)) 

startIdent :: GenParser GphTokenPos st ArithExpr 
startIdent = do
                i <- anyIdent
                do
                    do
                        s <- subprogCall i
                        return (ArithTerm (SubcallTerm s))
                    <|> 
                    return (ArithTerm (IdTerm i))

{- Subprogram calls.
 -
 - -}
subprogCall :: Identifier -> GenParser GphTokenPos st SubprogCall
subprogCall i = do
                (tok GTokLParen)
                es <- arithExprList -- change to anyExprList
                (tok GTokRParen)
                return (SubprogCall i es)
                

{- Stmt lists.
 -
 -}
identList :: GenParser GphTokenPos st [Identifier]
identList = do
                i <- anyIdent
                do
                    (tok GTokComma)
                    next <- identList
                    return (i : next)
                    <|> return [i]

arithExprList :: GenParser GphTokenPos st [ArithExpr]
arithExprList = do
                    e <- arithExpr
                    do
                        (tok GTokComma)
                        next <- arithExprList
                        return (e:next)
                        <|> return [e]

{- Arithmetic expressions parser.
 - 
 - -}

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
                    f <- factorArithExpr
                    do
                        termArithExprAux f
                        <|> return f

{--
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
--}
--
opOne :: GenParser GphTokenPos st ArithBinOp
opOne = do (tok GTokModulus) >> return ModBinOp 
        <|> 
        do (tok GTokDivision) >> return DivBinOp 
        <|> 
        do (tok GTokTimes) >> return TimesBinOp
        <|>
        do (tok GTokHat) >> return ExpBinOp
        <|>
        do (tok GTokPlusPlus) >> return PlusPlusBinOp
        <|>
        do (tok GTokTimesTimes) >> return TimesTimesBinOp
            

termArithExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
termArithExprAux t = do 
                        op <- opOne
                        f <- factorArithExpr
                        do
                            termArithExprAux (ArithBinExpr op t f)
                            <|> return (ArithBinExpr op t f)
 
factorArithExpr :: GenParser GphTokenPos st ArithExpr
factorArithExpr =   do
                        l <- literalArithExpr
                        do
                            do
                                (tok GTokHat)
                                f <- factorArithExpr
                                return (ArithBinExpr ExpBinOp l f)
                            <|>
                            do
                                return l
                        

literalArithExpr :: GenParser GphTokenPos st ArithExpr
literalArithExpr =  do
                        basisArithExpr
                    <|>
                    do
                        op <- opUnary
                        b <- basisArithExpr
                        return (ArithUnExpr op b)
                        

basisArithExpr :: GenParser GphTokenPos st ArithExpr
basisArithExpr = do 
                    n <- (numberLit)
                    return (ArithTerm (LitTerm n))
                  <|>
                  do
                    startIdent
                  <|>
                  do 
                    i <- (stringLit)
                    return (ArithTerm (LitTerm i))
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


