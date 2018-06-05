module Syntactic.Parser where

import Syntactic.Lexer
import Text.ParserCombinators.Parsec
import Syntactic.GTok
import Syntactic.GphTokens
import Execution.Memory
import Execution.Semantic
import Syntactic.Values
import Syntactic.Syntax

gryphParser :: GenParser GphTokenPos st [ProgramUnit]
gryphParser = 
    do result <- many (programUnit)
       return result

programUnit :: GenParser GphTokenPos st ProgramUnit
programUnit = do 
                    do
                        s <- structDecl
                        return (StructDecl s)
                    <|>
                    do 
                        s <- stmt
                        return (Stmt s)
                    <|>
                    do
                        s <- subprogDecl
                        return (SubprogramDecl s)

{- Structs.
 -
 --}
structDecl :: GenParser GphTokenPos st StructDecl
structDecl = do
                t <- userType
                (tok GTokLCurly)
--                d <- declStmtList
                d <- varDeclList GTokSemicolon
                (tok GTokRCurly)
                return (Struct t d)

structInit :: GenParser GphTokenPos st StructInit
structInit = do
                (tok GTokLCurly)
                is <- identAssignmentList
                (tok GTokRCurly)
                return (StructInit is)

{- Stmts.
 -
 -}

stmt :: GenParser GphTokenPos st Stmt
stmt = do
            matchedStmt <|> unmatchedStmt

stmtList :: GenParser GphTokenPos st [Stmt]
stmtList = do
                s <- stmt
                do 
                    next <- stmtList
                    return (s:next)
                    <|> return [s]

declStmtList :: GenParser GphTokenPos st [Stmt]
declStmtList = do
                s <- declStmt
                (tok GTokSemicolon)
                do 
                    next <- declStmtList
                    return (s:next)
                    <|> return [s]


stmtBlock :: GenParser GphTokenPos st Block
stmtBlock = do
                (tok GTokLCurly)
                ss <- stmtList
                (tok GTokRCurly)
                return (Block ss)

blockOrStmt :: GenParser GphTokenPos st CondBody
blockOrStmt =   do 
                    b <- stmtBlock 
                    return (CondBlock b)
                <|>
                do 
                    ms <- matchedStmt
                    return (CondStmt ms)

matchedStmt :: GenParser GphTokenPos st Stmt
matchedStmt = do
                matchedIfElse <|> commonStmt <|> forStmt <|> whileStmt <|> bfsStmt <|> dfsStmt

unmatchedStmt :: GenParser GphTokenPos st Stmt
unmatchedStmt = do
                ifStmt <|> unmatchedIfElse

commonStmt :: GenParser GphTokenPos st Stmt
commonStmt = do 
                s <- (readStmt <|> printStmt <|> attrStmt <|> subprogCallStmt <|> declStmt <|> returnStmt)
                (tok GTokSemicolon)
                return s

subprogCallStmt :: GenParser GphTokenPos st Stmt
subprogCallStmt = do
                        s <- subprogCall
                        return (SubCallStmt s)

returnStmt :: GenParser GphTokenPos st Stmt
returnStmt = do
                (tok GTokReturn)
                e <- expression
                return (ReturnStmt e)

--startIdentListStmt :: GenParser GphTokenPos st Stmt
--startIdentListStmt = do
--                    i <- identList
--                    do
--                        attrStmt i <|> declStmtAux i

attrStmt :: GenParser GphTokenPos st Stmt
attrStmt = do 
                do
                    es <- try $ 
                            do 
                                es <- postfixExprList
                                (tok GTokAssignment)
                                return es
                    vs <- expressionList
                    return (AttrStmt es vs)

declStmt :: GenParser GphTokenPos st Stmt
declStmt = do
                    i <- identList
                    do
                        declStmtAux i

declStmtAux :: [Identifier] -> GenParser GphTokenPos st Stmt
declStmtAux is = do
                    (tok GTokColon)
                    --t <- gryphParamType
                    t <- gryphType
                    do
                        do
                            (tok GTokAssignment)
                            es <- expressionList
                            return (DeclStmt (VarDeclaration is t es))
                        <|>
                        return (DeclStmt (VarDeclaration is t []))
                    

readStmt :: GenParser GphTokenPos st Stmt
readStmt = do 
                (tok GTokRead) 
                i <- anyIdent
                return (ReadStmt i) 

printStmt :: GenParser GphTokenPos st Stmt
printStmt = do
                (tok GTokPrint) 
                e <- expression
                return (PrintStmt e)

startIdent :: GenParser GphTokenPos st ArithExpr 
startIdent = do
                i <- anyIdent
                do
                    do
                        s <- subprogCallAux i
                        return (ArithTerm (SubcallTerm s))
                    <|> 
                    return (ArithTerm (IdTerm i))

varDecl :: GenParser GphTokenPos st VarDeclaration
varDecl = do
                i <- identList
                (tok GTokColon)
                t <- gryphType
                --t <- gryphParamType
                do
                    do
                        (tok GTokAssignment)
                        e <- expressionList
                        return (VarDeclaration i t e)
                    <|>
                    return (VarDeclaration i t [])
                
varDeclList :: GphToken -> GenParser GphTokenPos st [VarDeclaration]
varDeclList sep = do
                        a <- varDecl
                        (tok sep)
                        do
                            do
                                next <- varDeclList sep
                                return (a:next)
                            <|>
                            do
                                return [a]

paramDecl :: GenParser GphTokenPos st ParamDeclaration
paramDecl = do
                i <- identList
                (tok GTokColon)
                --t <- gryphType
                t <- gryphParamType
                do
                    do
                        (tok GTokAssignment)
                        e <- expressionList
                        return (ParamDeclaration i t e)
                    <|>
                    return (ParamDeclaration i t [])
                
paramDeclList :: GphToken -> GenParser GphTokenPos st [ParamDeclaration]
paramDeclList sep = do
                        a <- paramDecl
                        do
                            do
                                (tok sep)
                                next <- paramDeclList sep
                                return (a:next)
                            <|>
                                return [a]


{- Subprograms
 -
 -}
subprogDecl :: GenParser GphTokenPos st Subprogram
subprogDecl = do
                    (tok GTokSub)
                    i <- anyIdent
                    (tok GTokLParen)
                    do
                        do
                            (tok GTokRParen)
                            subprogDeclAux i []
                        <|>
                        do
                            ds <- paramDeclList (GTokSemicolon)
                            (tok GTokRParen)
                            subprogDeclAux i ds

subprogDeclAux :: Identifier -> [ParamDeclaration] -> GenParser GphTokenPos st Subprogram
subprogDeclAux i ds = do         
                            do
                                (tok GTokColon)
                                t <- gryphType 
                                --t <- gryphParamType 
                                b <- stmtBlock
--                                declareSubprogram ((\(Ident i)->i)i, extractFormalTypes ds) (extractFormalDecls ds, (Just t), b)                                 
                                return (Subprogram i ds (Just t) b)
                                --return (Function i ds t b)
                            <|>
                            do
                                b <- stmtBlock
                                return (Subprogram i ds Nothing b)
                                --return (Procedure i ds b)

                    
{- Literals
 -
 -}
listLit :: GenParser GphTokenPos st ArithExpr
listLit = do
                (tok GTokLSquare)
                do
                    do 
                        try $ do
                            lc <- listComp
                            (tok GTokRSquare)
                            return (ExprLiteral (ListCompLit lc))
                    <|>
                    do
                        (tok GTokRSquare)
                        return (ExprLiteral (ListLit [])) 
                    <|>
                    do
                        l <- expressionList
                        (tok GTokRSquare)
                        return (ExprLiteral (ListLit l))

tupleLit :: GenParser GphTokenPos st ArithExpr
tupleLit = do
                (tok GTokLParen)
                e1 <- expression
                (tok GTokComma)
                e2 <- expression
                do
                    do
                        (tok GTokRParen)
                        return (ExprLiteral (TupleLit (e1:[e2])))
                    <|>
                    do
                        (tok GTokComma)
                        l <- expressionList
                        (tok GTokRParen)
                        return (ExprLiteral (TupleLit (e1:(e2:l))))

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

{- BFS Stmt
 -
 - -}
bfsStmt :: GenParser GphTokenPos st Stmt
bfsStmt = do
            (tok GTokBFS)
            is <- identList
            (tok GTokIn)
            g <- expression
            do
                do
                    (tok GTokFrom)
                    v <- expression
                    b <- blockOrStmt
                    return (BfsStmt is g (Just v) b)
                <|>
                do
                    b <- blockOrStmt
                    return (BfsStmt is g Nothing b)
{- DFS Stmt
 -
 - -}
dfsStmt :: GenParser GphTokenPos st Stmt
dfsStmt = do
            (tok GTokDFS)
            is <- identList
            (tok GTokIn)
            g <- expression
            do
                do
                    (tok GTokFrom)
                    v <- expression
                    b <- blockOrStmt
                    return (DfsStmt is g (Just v) b)
                <|>
                do
                    b <- blockOrStmt
                    return (DfsStmt is g Nothing b)
            
{- Graphs.
 -
 -}
graphLit :: GenParser GphTokenPos st ArithExpr
graphLit = do
                (tok GTokLess)
                v <- primaryExpr
                do
                    do
                        (tok GTokComma)
                        ec <- edgeComp
                        (tok GTokGreater)
                        return (ExprLiteral (GraphLit (Just v) (Just ec)))
                    <|>
                    do 
                        (tok GTokGreater)
                        return (ExprLiteral (GraphLit (Just v) Nothing))

edgeComp :: GenParser GphTokenPos st EdgeComp
edgeComp = do
                do
                    ex <- try $ do
                            es <- expression
                            (tok GTokWhere)
                            return es
                    ed <- edge
                    f <- forIterator
                    return (EdgeComp (Just ex) ed f)
                <|>
                do
                    ed <- edge
                    f <- forIterator
                    return (EdgeComp Nothing ed f)
                        
                    
edge :: GenParser GphTokenPos st Edge
edge = do
            e1 <- expression
            t <- edgeType
            e2 <- expression
            return (Edge t e1 e2)

edgeType :: GenParser GphTokenPos st EdgeType
edgeType = do (tok GTokRightEdge) >> return (RightEdge)
            <|> do (tok GTokLeftEdge) >> return (LeftEdge)
            <|> do (tok GTokDoubleEdge) >> return (DoubleEdge)

{- While stmt.
 -
 --}
whileStmt :: GenParser GphTokenPos st Stmt
whileStmt = do
                (tok GTokWhile)
                (tok GTokLParen)
                be <- expression
                (tok GTokRParen)
                b <- blockOrStmt
                return (WhileStmt be b)
                

{- For stmt.
 -
 -
 -}
forStmt :: GenParser GphTokenPos st Stmt
forStmt = do
                (tok GTokFor)
                is <- identList
                (tok GTokOver)
                es <- expressionList
                b <- blockOrStmt
                return (ForStmt is es b)
                
forIterator :: GenParser GphTokenPos st ForIterator
forIterator = do
                (tok GTokFor)
                is <- identList
                (tok GTokOver)
                es <- expressionList
                do
                    do
                        (tok GTokWhen)
                        bs <- expressionList
                        return (ForIterator is es bs)
                    <|>
                    do
                        return (ForIterator is es [])

                

listComp :: GenParser GphTokenPos st ListComp
listComp = do
                e <- expression
                f <- forIterator 
                return (ListComp e f)

{- If stmt.
 -
 -}
ifExpr :: GenParser GphTokenPos st ArithExpr
ifExpr = do
            (tok GTokIf)
            (tok GTokLParen)
            e <- expression
            (tok GTokRParen)
            return e

ifStmt :: GenParser GphTokenPos st Stmt
ifStmt = do
                e <- ifExpr
                s <- stmt
                (tok GTokSemicolon)
                return (IfStmt e (IfBody (CondStmt s)) NoElse)

unmatchedIfElse :: GenParser GphTokenPos st Stmt
unmatchedIfElse = do
                        e <- ifExpr
                        ms <- matchedStmt
                        (tok GTokSemicolon)
                        (tok GTokElse)
                        us <- unmatchedStmt
                        return (IfStmt e (IfBody (CondStmt ms)) (ElseBody (CondStmt us)))
                        
matchedIfElse :: GenParser GphTokenPos st Stmt
matchedIfElse = do
                    e <- ifExpr
                    b1 <- blockOrStmt
                    do
                        do
                            (tok GTokElse)
                            b2 <- blockOrStmt
                            return (IfStmt e (IfBody b1) (ElseBody b2))
                        <|>
                            return (IfStmt e (IfBody b1) NoElse)

{- Subprogram calls.
 -
 - -}

identAssignment :: GenParser GphTokenPos st IdentAssign
identAssignment = do
                    i <- try $ do 
                            i <- anyIdent
                            (tok GTokAssignment)
                            return [i]
                    e <- expression
                    return (IdentAssign i e)

identAssignmentList :: GenParser GphTokenPos st [IdentAssign]
identAssignmentList = do
                            a <- identAssignment
                            do
                                do
                                    (tok GTokComma)
                                    next <- identAssignmentList
                                    return (a : next)
                                <|>
                                do
                                    return [a]

subprogArg :: GenParser GphTokenPos st SubprogArg
subprogArg = do
                do
                    i <- identAssignment 
                    return (ArgIdentAssign i) 
                <|> 
                do 
                    e <- expression
                    return (ArgExpr e)

subprogArgList :: GenParser GphTokenPos st [SubprogArg]
subprogArgList = do
                    s <- subprogArg
                    do
                        do
                            (tok GTokComma)
                            next <- subprogArgList
                            return (s:next)
                        <|>
                        do
                            return [s]

subprogCall :: GenParser GphTokenPos st SubprogCall
subprogCall = do
                try $ do
                    i <- anyIdent
                    subprogCallAux i

subprogCallAux :: Identifier -> GenParser GphTokenPos st SubprogCall
subprogCallAux i = do
                    (tok GTokLParen)
                    do
                        do
                            (tok GTokRParen)
                            return (SubprogCall i [])
                        <|> 
                            do
                                es <- subprogArgList 
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
                --t <- gryphParamType
                t <- gryphType
                do
                    do
                        (tok GTokComma)
                        next <- typeList
                        return (t : next)
                    <|> return [t]
                    
gryphParamType :: GenParser GphTokenPos st GParamType
gryphParamType = do
                    t <- gryphType
                    do
                        do
                            (tok GTokAmpersand)
                            return (GRef t)
                        <|> return (GType t)

gryphType :: GenParser GphTokenPos st GType
gryphType = nativeType <|> userType

nativeType :: GenParser GphTokenPos st GType
nativeType = primitiveType <|> compositiveType

compositiveType :: GenParser GphTokenPos st GType
compositiveType = do
                        do
                            (tok GTokLSquare)
                            t <- gryphType
                            --t <- gryphParamType
                            (tok GTokRSquare)
                            return (GList t)
                        <|>
                        do
                            (tok GTokPipe)
                            t <- gryphType
                            --t <- gryphParamType
                            (tok GTokComma)
                            a <- gryphType
                            --a <- gryphParamType
                            (tok GTokPipe)
                            return (GDict t a)
                        <|>
                        do
                            (tok GTokLParen)
                            do
                                t1 <- gryphType
                                --t1 <- gryphParamType
                                (tok GTokComma)
                                t2 <- gryphType
                                --t2 <- gryphParamType
                                do
                                    do
                                        (tok GTokComma)
                                        t3 <- gryphType
                                        --t3 <- gryphParamType
                                        do
                                            do 
                                                (tok GTokComma)
                                                t4 <- gryphType
                                                --t4 <- gryphParamType
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
                        _ -> return (GUserType t)

graphType :: GenParser GphTokenPos st GType
graphType = do
                (tok GTokLess)
                --t <- gryphParamType
                t <- gryphType
                do
                    do
                        (tok GTokComma)
                        a <- gryphType
                        --a <- gryphParamType
                        (tok GTokGreater)
                        return (GGraphVertexEdge t a)
                    <|>
                    do  
                        (tok GTokGreater)
                        return (GGraphVertex t)


userType :: GenParser GphTokenPos st GType
userType = do
                t <- anyType
                return (GUserType t)


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
                    try $ do
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
                        --t <- gryphParamType
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

postfixExprList :: GenParser GphTokenPos st [ArithExpr]
postfixExprList = do
                        e <- postfixExpr
                        do
                            do
                                (tok GTokComma)
                                next <- postfixExprList
                                return (e:next)
                            <|>
                            return [e]

postfixExpr :: GenParser GphTokenPos st ArithExpr
postfixExpr = do
                    do
                        try $ do
                            i <- primaryExpr
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
                    do
                        try $ do
                            tupleLit
                    <|>
                    do
                        (tok GTokLParen)
                        e <- expression
                        (tok GTokRParen)
                        return e
                    <|>
                    do
                        e <- structInit
                        return (StructInitExpr e)
                    <|> 
                        startIdent -- ident or subprogcall
                    <|> constant <|> listLit <|> dictLit <|> graphLit 
                    

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

parseFile :: String -> IO [ProgramUnit]
parseFile file = 
    do program <- readFile file
       case parse gryphParser "" (alexScanTokens program) of
            Left e  -> print e >> fail "parse error"
            Right r -> return r

