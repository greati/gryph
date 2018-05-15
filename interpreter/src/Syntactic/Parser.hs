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
    <|> ifStmt

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

{- If stmt.
 -
 -}
ifStmt :: GenParser GphTokenPos st Stmt
ifStmt = do
            (tok GTokIf)
            (tok GTokLParen)
            e <- boolExpr
            (tok GTokRParen)
            return (IfStmt e)


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

{--
 - Relational expressions parser.
 -
 -}
relOp :: GenParser GphTokenPos st RelOp
relOp = do (tok GTokGreater) >> return Greater
        <|>
        do (tok GTokLess) >> return Less
        <|>
        do (tok GTokLessEq) >> return LessEq
        <|> 
        do (tok GTokGreaterEq) >> return GreaterEq
        <|>
        do (tok GTokEq) >> return Equals
        <|>
        do (tok GTokNeq) >> return NotEquals
        

relExpr :: AnyExpr -> GenParser GphTokenPos st RelExpr
relExpr e = do 
                relExprAux e

relExprAux :: AnyExpr -> GenParser GphTokenPos st RelExpr
relExprAux r = do
                op <- relOp
                t <- relTerm
                do
                    do
                        relExprAux (RelExpr (BinRelExpr op r t))
                    <|>
                    do
                        return (BinRelExpr op r t)

relTerm :: GenParser GphTokenPos st AnyExpr
relTerm = do
            e <- arithExpr 
            return (ArithExpr e)

{- Boolean expression parser.
 -
 --}
boolUnOp :: GenParser GphTokenPos st BoolUnOp
boolUnOp = do (tok GTokNot) >> return (Not)

boolOp0 :: GenParser GphTokenPos st BoolBinOp
boolOp0 = do (tok GTokOr) >> return (Or)
          <|>
          do (tok GTokXor) >> return (Xor)

boolOp1 :: GenParser GphTokenPos st BoolBinOp
boolOp1 = do (tok GTokAnd) >> return (And)

boolExpr :: GenParser GphTokenPos st BoolExpr
boolExpr = do
                t <- boolTerm
                do
                    do  
                        boolExprAux t
                    <|>
                    do
                        return t

boolExprAux :: BoolExpr -> GenParser GphTokenPos st BoolExpr
boolExprAux e = do
                    op <- boolOp0
                    t <- boolTerm
                    do
                        do
                            boolExprAux (BoolBinExpr op e t)
                        <|>
                        do
                            return (BoolBinExpr op e t)
                        
boolTerm :: GenParser GphTokenPos st BoolExpr
boolTerm = do
                l <- boolLiteral 
                do
                    do
                        boolTermAux l
                    <|>
                    do
                        return l

boolTermAux :: BoolExpr -> GenParser GphTokenPos st BoolExpr
boolTermAux e = do
                    op <- boolOp1
                    l <- boolLiteral
                    do
                        do
                            boolTermAux (BoolBinExpr op e l)
                        <|>
                        do
                            return (BoolBinExpr op e l)

boolLiteral :: GenParser GphTokenPos st BoolExpr
boolLiteral = do
                    do
                        op <- boolUnOp
                        b <- boolBase
                        return (BoolUnExpr op b)
                    <|>
                    do
                        boolBase

boolBase :: GenParser GphTokenPos st BoolExpr
boolBase = do
                do
                    (tok GTokLParen)
                    e <- boolExpr
                    (tok GTokRParen)
                    return e
                <|>
                do
                    (tok GTokTrue)
                    return LitTrue
                <|>
                do
                    (tok GTokFalse)
                    return LitFalse
                <|> 
                do
                    e <- startIdent 
                    do
                        do 
                            a <- arithExprAux e
                            r <- relExpr (ArithExpr a)
                            return (BoolRelExpr r)
                        <|>
                        do
                            r <- relExpr (ArithExpr e)
                            return (BoolRelExpr r)
                        <|>
                        do
                            case e of
                                ArithTerm (SubcallTerm s) -> return (BoolSubcallTerm s)
                                ArithTerm (IdTerm i) -> return (BoolIdTerm i)
                <|>
                do
                    e <- arithExpr
                    r <- relExpr (ArithExpr e)
                    return (BoolRelExpr r)


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



