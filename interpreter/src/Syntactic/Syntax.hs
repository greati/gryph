module Syntactic.Syntax where

import Syntactic.Values

import qualified Text.Read as TRead

type Type = String
    
data GType =    GInteger        |
                GFloat          |
                GString         |
                GChar           |
                GBool           |
                GList GType     |
                GPair GType GType   |
                GTriple GType GType GType  |
                GQuadruple GType GType GType GType |
                GDict GType GType |
                GGraphVertex GType |
                GGraphVertexEdge GType GType |
                GUserType Identifier
                deriving (Show, Eq)

instance Read GType where
    readPrec = (TRead.prec 10 $
                    do 
                        TRead.Ident s <- TRead.lexP
                        case s of
                            "int" -> return GInteger
                            "float" -> return GFloat
                            "string" -> return GString
                            "char" -> return GChar
                            "bool" -> return GBool
                            )
                TRead.+++
                (TRead.prec 5 $
                    do
                        TRead.Punc s <- TRead.lexP
                        case s of 
                            "[" -> do
                                        a <- TRead.readPrec
                                        TRead.Punc "]" <- TRead.lexP
                                        return (GList a)
                    )

data ProgramUnit = Stmt Stmt | Subprogram Subprogram deriving (Show, Eq)

type GTypeList = [GType]

data VarDeclaration = VarDeclaration [Identifier] GType [ArithExpr] deriving (Show, Eq)

data Subprogram = Function Identifier [VarDeclaration] GType Block | 
                Procedure Identifier [VarDeclaration] Block deriving (Show, Eq)

data Identifier = Ident String deriving(Show, Eq)

data Literal = Lit Value deriving(Show, Eq)
type DictEntry = (ArithExpr, ArithExpr)
data ExprLiteral = ListLit [ArithExpr] | TupleLit [ArithExpr] | DictLit [DictEntry] deriving (Show, Eq)

data SubprogCall = SubprogCall Identifier [ArithExpr] deriving(Show, Eq) -- change to anyExpr

data Stmt = ReadStmt Identifier | 
            PrintStmt Term | 
            DeclStmt VarDeclaration | --[Identifier] GType [ArithExpr] | 
            AttrStmt [Identifier] [ArithExpr] |
            IfStmt ArithExpr IfBody ElseBody |
            ReturnStmt ArithExpr
            deriving (Show, Eq)

data IfBody =  IfBody CondBody deriving(Eq, Show)
data ElseBody = NoElse | ElseBody CondBody deriving(Eq, Show)

data Block = Block [Stmt] deriving(Show, Eq)

data CondBody = CondStmt Stmt | CondBlock Block deriving(Eq, Show)

data ArithUnOp =    MinusUnOp | 
                    PlusUnOp |
                    NotUnOp
                    deriving (Show, Eq)

data ArithBinOp =   MinusBinOp | 
                    PlusBinOp | 
                    TimesBinOp | 
                    DivBinOp | 
                    ModBinOp | 
                    ExpBinOp |
                    PlusPlusBinOp | 
                    TimesTimesBinOp 
                    deriving (Show, Eq)

data RelOp = 
            Greater |
            Less |
            GreaterEq|    
            LessEq 
            deriving (Show, Eq)

data EqOp =
            Equals |
            NotEquals 
            deriving (Show, Eq)
            

data BoolUnOp = Not deriving (Show, Eq)

data BoolBinOp = 
            And |
            Or |
            Xor 
            deriving (Show, Eq)

data BoolExpr = BoolBinExpr BoolBinOp BoolExpr BoolExpr |
                BoolUnExpr BoolUnOp BoolExpr |
                LitTrue |
                LitFalse |
                BoolRelExpr RelExpr |
                BoolIdTerm Identifier |
                BoolSubcallTerm SubprogCall
                deriving (Show, Eq)

data AnyExpr = RelExpr RelExpr | ArithExpr ArithExpr | BoolExpr BoolExpr deriving (Show, Eq)

data RelExpr = BinRelExpr RelOp AnyExpr AnyExpr deriving (Show, Eq)

data Term = LitTerm Literal | 
            IdTerm Identifier |
            SubcallTerm SubprogCall 
            deriving (Show, Eq)

data ArithExpr =    ArithUnExpr ArithUnOp ArithExpr | 
                    ArithBinExpr ArithBinOp ArithExpr ArithExpr | 
                    ArithTerm Term |
                    ExprLiteral ExprLiteral |
                    GraphAccess Identifier ArithExpr |
                    DictAccess Identifier ArithExpr |
                    ListAccess Identifier ArithExpr |
                    StructAccess Identifier ArithExpr |
                    TupleAccess Identifier ArithExpr |
                    CastExpr ArithExpr GType |
                    ArithRelExpr RelOp ArithExpr ArithExpr |
                    ArithEqExpr EqOp ArithExpr ArithExpr |
                    LogicalBinExpr BoolBinOp ArithExpr ArithExpr
                    deriving (Show, Eq)

data IdentList = IdentList [Identifier] deriving (Show, Eq)

