module Syntactic.Syntax where

import Syntactic.Values

type Type = String

data Identifier = Ident String deriving(Show, Eq)

data Literal = Lit Value deriving(Show, Eq)

data SubprogCall = SubprogCall Identifier [ArithExpr] deriving(Show, Eq) -- change to anyExpr

data Stmt = ReadStmt Identifier | 
            PrintStmt Term | 
            DeclStmt [Identifier] Type [ArithExpr] | 
            AttrStmt [Identifier] [ArithExpr] |
            IfStmt ArithExpr
            deriving (Show, Eq)
    

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
                    GraphAccess Identifier ArithExpr |
                    DictAccess Identifier ArithExpr |
                    ListAccess Identifier ArithExpr |
                    StructAccess Identifier ArithExpr |
                    TupleAccess Identifier ArithExpr |
                    CastExpr ArithExpr Type |
                    ArithRelExpr RelOp ArithExpr ArithExpr |
                    ArithEqExpr EqOp ArithExpr ArithExpr |
                    LogicalBinExpr BoolBinOp ArithExpr ArithExpr
                    deriving (Show, Eq)

data IdentList = IdentList [Identifier] deriving (Show, Eq)

