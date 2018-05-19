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
                vs <- expressionList
                (tok GTokSemicolon)
                return (AttrStmt is vs)

declStmt :: [Identifier] -> GenParser GphTokenPos st Stmt
declStmt is = do
                (tok GTokColon)
                t <- gryphType
                do
                    (tok GTokSemicolon)
                    return (DeclStmt is t [])
                    <|>
                    do
                        (tok GTokAssignment)
                        es <- expressionList
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

{- Literals
 -
 -}
listLit :: GenParser GphTokenPos st ArithExpr
listLit = do
                (tok GTokLSquare)
                l <- expressionList
                (tok GTokRSquare)
                return (ExprLiteral (ListLit l))

tupleLit :: GenParser GphTokenPos st ArithExpr
tupleLit = do
                (tok GTokLParen)
                l <- expressionList
                (tok GTokRParen)
                return (ExprLiteral (TupleLit l))

dictEntry :: GenParser GphTokenPos st DictEntry
dictEntry = do
                k <- expression
                (tok GTokQuestion)
                v <- expression
                return (k,v)

dictEntryList :: GenParser GphTokenPos st [DictEntry]
dictEntryList = do
                    d <- dictEntry
                    do
                        do
                            (tok GTokComma)
                            next <- dictEntryList
                            return (d : next)
                        <|> return [d]

dictLit :: GenParser GphTokenPos st ArithExpr
dictLit = do
                    (tok GTokPipe)
                    l <- dictEntryList 
                    (tok GTokPipe)
                    return (ExprLiteral (DictLit l))

{- If stmt.
 -
 -}
ifStmt :: GenParser GphTokenPos st Stmt
ifStmt = do
            (tok GTokIf)
            (tok GTokLParen)
            e <- expression
            (tok GTokRParen)
            return (IfStmt e)


{- Subprogram calls.
 -
 - -}
subprogCall :: Identifier -> GenParser GphTokenPos st SubprogCall
subprogCall i = do
                (tok GTokLParen)
                es <- expressionList -- change to anyExprList
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


{-
 - Type names.
-}

typeList :: GenParser GphTokenPos st GTypeList
typeList = do
                t <- gryphType
                do
                    do
                        (tok GTokComma)
                        next <- typeList
                        return (t : next)
                    <|> return [t]
                    

gryphType :: GenParser GphTokenPos st GType
gryphType = nativeType <|> userType

nativeType :: GenParser GphTokenPos st GType
nativeType = primitiveType <|> compositiveType

compositiveType :: GenParser GphTokenPos st GType
compositiveType = do
                        do
                            (tok GTokLSquare)
                            t <- gryphType
                            (tok GTokRSquare)
                            return (GList t)
                        <|>
                        do
                            (tok GTokPipe)
                            t <- gryphType
                            (tok GTokComma)
                            a <- gryphType
                            (tok GTokPipe)
                            return (GDict t a)
                        <|>
                        do
                            (tok GTokLParen)
                            do
                                t1 <- gryphType
                                (tok GTokComma)
                                t2 <- gryphType
                                do
                                    do
                                        (tok GTokComma)
                                        t3 <- gryphType
                                        do
                                            do 
                                                (tok GTokComma)
                                                t4 <- gryphType
                                                (tok GTokRParen)
                                                return (GQuadruple t1 t2 t3 t4)
                                            <|>
                                            do
                                                (tok GTokRParen)
                                                return (GTriple t1 t2 t3)
                                    <|>
                                    do
                                        (tok GTokRParen)
                                        return (GPair t1 t2)
                        <|>
                        graphType
        

primitiveType :: GenParser GphTokenPos st GType
primitiveType = do
                    t <- anyType
                    case t of
                        "int" -> return GInteger
                        "float" -> return GFloat
                        "string" -> return GString
                        "char" -> return GChar
                        "bool" -> return GBool
                        _ -> return (GUserType (Ident t))

graphType :: GenParser GphTokenPos st GType
graphType = do
                (tok GTokLess)
                t <- gryphType
                do
                    do
                        (tok GTokComma)
                        a <- gryphType
                        (tok GTokGreater)
                        return (GGraphVertexEdge t a)
                    <|>
                    do  
                        (tok GTokGreater)
                        return (GGraphVertex t)


userType :: GenParser GphTokenPos st GType
userType = do
                t <- anyType
                return (GUserType (Ident t))


{- New expression parser.
 -
 -
 -}


expression :: GenParser GphTokenPos st ArithExpr
expression = logicalXorExpr

logicalXorExpr :: GenParser GphTokenPos st ArithExpr
logicalXorExpr = do 
                     e <- logicalOrExpr
                     do
                         logicalXorExprAux e
                         <|> return e

logicalXorExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
logicalXorExprAux e = do
                        op <- boolXorOp
                        r <- logicalOrExpr
                        do
                            do
                                logicalXorExprAux (LogicalBinExpr op e r)
                                <|> return (LogicalBinExpr op e r) 

logicalOrExpr :: GenParser GphTokenPos st ArithExpr
logicalOrExpr = do 
                     e <- logicalAndExpr
                     do
                         logicalOrExprAux e
                         <|> return e

logicalOrExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
logicalOrExprAux e = do
                        op <- boolOrOp
                        r <- logicalAndExpr
                        do
                            do
                                logicalOrExprAux (LogicalBinExpr op e r)
                                <|> return (LogicalBinExpr op e r) 

logicalAndExpr :: GenParser GphTokenPos st ArithExpr
logicalAndExpr = do 
                     e <- eqExpr
                     do
                         logicalAndExprAux e
                         <|> return e

logicalAndExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
logicalAndExprAux e = do
                        op <- boolAndOp
                        r <- eqExpr
                        do
                            do
                                logicalAndExprAux (LogicalBinExpr op e r)
                                <|> return (LogicalBinExpr op e r) 
 

eqExpr :: GenParser GphTokenPos st ArithExpr
eqExpr = do 
             e <- relExpr
             do
                 eqExprAux e
                 <|> return e

eqExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
eqExprAux e = do
                    op <- eqOp
                    r <- relExpr
                    do
                        do
                            eqExprAux (ArithEqExpr op e r)
                            <|> return (ArithEqExpr op e r) 
 

relExpr :: GenParser GphTokenPos st ArithExpr
relExpr = do 
                e <- addExpr
                do
                    relExprAux e
                    <|> return e

relExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
relExprAux e = do
                    op <- relOp
                    a <- addExpr
                    do
                        do
                            relExprAux (ArithRelExpr op e a)
                            <|> return (ArithRelExpr op e a) 
                        

addExpr :: GenParser GphTokenPos st ArithExpr
addExpr = do
                e <- multExpr
                do
                    addExprAux e
                    <|> return e

addExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
addExprAux e = do
                    op <- opAdd
                    r <- multExpr
                    do
                        do
                            addExprAux (ArithBinExpr op e r)
                            <|> return (ArithBinExpr op e r) 
                            
multExpr :: GenParser GphTokenPos st ArithExpr
multExpr = do
                e <- expExpr
                do
                    multExprAux e
                    <|> return e

multExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
multExprAux e = do
                    op <- opMult
                    r <- expExpr
                    do
                        do
                            multExprAux (ArithBinExpr op e r)
                            <|> return (ArithBinExpr op e r) 
                            

expExpr :: GenParser GphTokenPos st ArithExpr
expExpr = do
                c <- castExpr
                do
                    do
                        op <- opExp
                        e <- expExpr
                        return (ArithBinExpr op c e)
                    <|> 
                    do
                        return c
                    

castExpr :: GenParser GphTokenPos st ArithExpr
castExpr = 
            do
                e <- unaryExpr
                do
                    castExprAux e
                    <|> return e

castExprAux :: ArithExpr -> GenParser GphTokenPos st ArithExpr
castExprAux e = do
                    do
                        (tok GTokAt)
                        t <- gryphType
                        do
                            do
                                castExprAux (CastExpr e t)
                            <|>
                            do
                                return (CastExpr e t)
                        

unaryExpr :: GenParser GphTokenPos st ArithExpr
unaryExpr = do
                do
                    op <- opUnary
                    e <- castExpr
                    return (ArithUnExpr op e)
                <|>
                do
                    postfixExpr

postfixExpr :: GenParser GphTokenPos st ArithExpr
postfixExpr = do
                    try $ do
                        i <- anyIdent
                        do
                            do
                                (tok GTokLess) 
                                e <- expression
                                (tok GTokGreater)
                                return (GraphAccess i e)
                            <|>
                            do
                                (tok GTokPipe) 
                                e <- expression
                                (tok GTokPipe)
                                return (DictAccess i e)
                            <|>
                            do
                                (tok GTokLSquare) 
                                e <- expression
                                (tok GTokRSquare)
                                return (ListAccess i e)
                            <|>
                            do
                                (tok GTokLCurly) 
                                e <- expression
                                (tok GTokRCurly)
                                return (StructAccess i e)
                            <|>
                            do
                                (tok GTokDot) 
                                e <- expression
                                return (TupleAccess i e)
                    <|>
                    do
                        primaryExpr

primaryExpr :: GenParser GphTokenPos st ArithExpr
primaryExpr = do
                    try $ do
                        tupleLit
                    <|>
                    do
                        (tok GTokLParen)
                        e <- expression
                        (tok GTokRParen)
                        return e
                    <|> startIdent -- ident or subprogcall
                    <|> constant <|> listLit <|> dictLit 
                    

constant :: GenParser GphTokenPos st ArithExpr
constant = do
                do
                    n <- (numberLit)
                    return (ArithTerm (LitTerm n))
                <|>
                do 
                    i <- (stringLit)
                    return (ArithTerm (LitTerm i))
                <|>
                do
                    b <- boolLit
                    return (ArithTerm (LitTerm b))


boolLit :: GenParser GphTokenPos st Literal
boolLit = do
                do
                    (tok GTokTrue)
                    return (Lit (Bool True))
                <|>
                do
                    (tok GTokFalse)
                    return (Lit (Bool False))

opUnary :: GenParser GphTokenPos st ArithUnOp
opUnary = do (tok GTokPlus) >> return PlusUnOp
        <|>
        do (tok GTokMinus) >> return MinusUnOp
        <|>
        do (tok GTokNot) >> return NotUnOp

opAdd :: GenParser GphTokenPos st ArithBinOp
opAdd = do (tok GTokPlus) >> return PlusBinOp 
        <|>
        do (tok GTokMinus) >> return MinusBinOp

opMult :: GenParser GphTokenPos st ArithBinOp
opMult = do (tok GTokModulus) >> return ModBinOp 
        <|> 
        do (tok GTokDivision) >> return DivBinOp 
        <|> 
        do (tok GTokTimes) >> return TimesBinOp
        <|>
        do (tok GTokPlusPlus) >> return PlusPlusBinOp
        <|>
        do (tok GTokTimesTimes) >> return TimesTimesBinOp
            

opExp :: GenParser GphTokenPos st ArithBinOp
opExp = do (tok GTokHat) >> return ExpBinOp

relOp :: GenParser GphTokenPos st RelOp
relOp = do (tok GTokGreater) >> return Greater
        <|>
        do (tok GTokLess) >> return Less
        <|>
        do (tok GTokLessEq) >> return LessEq
        <|> 
        do (tok GTokGreaterEq) >> return GreaterEq

eqOp :: GenParser GphTokenPos st EqOp
eqOp =  do (tok GTokEq) >> return Equals
        <|>
        do (tok GTokNeq) >> return NotEquals
 
boolUnOp :: GenParser GphTokenPos st BoolUnOp
boolUnOp = do (tok GTokNot) >> return (Not)

boolOrOp :: GenParser GphTokenPos st BoolBinOp
boolOrOp = do (tok GTokOr) >> return (Or)

boolAndOp :: GenParser GphTokenPos st BoolBinOp
boolAndOp = do (tok GTokAnd) >> return (And)

boolXorOp :: GenParser GphTokenPos st BoolBinOp
boolXorOp = do (tok GTokXor) >> return (Xor)


expressionList :: GenParser GphTokenPos st [ArithExpr]
expressionList = do
                    e <- expression
                    do
                        (tok GTokComma)
                        next <- expressionList
                        return (e:next)
                        <|> return [e]

parseFile :: String -> IO [Stmt]
parseFile file = 
    do program <- readFile file
       case parse gryphParser "" (alexScanTokens program) of
            Left e  -> print e >> fail "parse error"
            Right r -> return r



