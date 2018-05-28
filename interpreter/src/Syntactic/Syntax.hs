module Syntactic.Syntax where

import Syntactic.Values

import qualified Text.Read as TRead

type Type = String

data GType =    GInteger        |
                GFloat          |
                GString         |
                GChar           |
                GBool           |
                GEmpty |
                GList GType     |
                GPair GType GType   |
                GTriple GType GType GType  |
                GQuadruple GType GType GType GType |
                GDict GType GType |
                GGraphVertex GType |
                GGraphVertexEdge GType GType |
                GUserType Identifier
                deriving (Show, Eq, Ord)

data GParamType = GType GType | GRef GType deriving (Show, Eq, Ord)

data ProgramUnit =  Stmt Stmt | 
                    SubprogramDecl Subprogram |
                    StructDecl StructDecl
                    deriving (Show, Eq)

data StructDecl = Struct GType [Stmt] deriving (Show, Eq)

data StructInit = StructInit [IdentAssign] deriving (Show, Eq)

type GTypeList = [GType]

data VarDeclaration = VarDeclaration [Identifier] GType [ArithExpr] deriving (Show, Eq)
data ParamDeclaration = ParamDeclaration [Identifier] GParamType [ArithExpr] deriving (Show, Eq)

data Subprogram = Subprogram Identifier [ParamDeclaration] (Maybe GType) Block 
                deriving (Show, Eq)

data Identifier = Ident String deriving(Show, Eq, Ord)

data IdentAssign = IdentAssign [Identifier] ArithExpr deriving (Show, Eq)

data Literal = Lit Value deriving(Show, Eq)
type DictEntry = (ArithExpr, ArithExpr)
data ExprLiteral =  ListLit [ArithExpr] | 
                    ListCompLit ListComp | 
                    TupleLit [ArithExpr] | 
                    DictLit [DictEntry] | 
                    GraphLit (Maybe ArithExpr) (Maybe EdgeComp) 
                    deriving (Show, Eq)

data SubprogArg = ArgIdentAssign IdentAssign | ArgExpr ArithExpr deriving (Show, Eq)

data SubprogCall = SubprogCall Identifier [SubprogArg] deriving(Show, Eq) -- change to anyExpr

data EdgeComp = EdgeComp (Maybe ArithExpr) Edge ForIterator deriving (Show, Eq)

data EdgeType = LeftEdge | RightEdge | DoubleEdge deriving (Show, Eq)

data Edge = Edge EdgeType ArithExpr ArithExpr deriving (Show, Eq)

data Stmt = ReadStmt Identifier | 
            PrintStmt ArithExpr | 
            DeclStmt VarDeclaration | --[Identifier] GType [ArithExpr] | 
            AttrStmt [ArithExpr] [ArithExpr] |
            SubCallStmt SubprogCall |
            IfStmt ArithExpr IfBody ElseBody |
            ReturnStmt ArithExpr |
            ForStmt [Identifier] [ArithExpr] CondBody |
            WhileStmt ArithExpr CondBody |
            BfsStmt [Identifier] ArithExpr (Maybe ArithExpr) CondBody |
            DfsStmt [Identifier] ArithExpr (Maybe ArithExpr) CondBody 
            deriving (Show, Eq) 

data ForIterator = ForIterator [Identifier] [ArithExpr] [ArithExpr] deriving (Show, Eq)

-- | a+1 for a,b over [1,2],[2,3] when a < 2 
data ListComp = ListComp ArithExpr ForIterator deriving (Show, Eq)

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
                    GraphAccess ArithExpr ArithExpr |
                    DictAccess ArithExpr ArithExpr |
                    ListAccess ArithExpr ArithExpr |
                    StructAccess ArithExpr ArithExpr |
                    TupleAccess ArithExpr ArithExpr |
                    CastExpr ArithExpr GType |
                    ArithRelExpr RelOp ArithExpr ArithExpr |
                    ArithEqExpr EqOp ArithExpr ArithExpr |
                    LogicalBinExpr BoolBinOp ArithExpr ArithExpr |
                    StructInitExpr StructInit
                    deriving (Show, Eq)

data IdentList = IdentList [Identifier] deriving (Show, Eq)

