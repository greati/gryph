module Syntactic.Syntax where

import Syntactic.Values

type Type = String

data Identifier = Ident String deriving(Show, Eq)

data Literal = Lit Value deriving(Show, Eq)

data SubprogCall = SubprogCall Identifier [ArithExpr] deriving(Show, Eq) -- change to anyExpr

data Stmt = ReadStmt Identifier | 
            PrintStmt Term | 
            DeclStmt [Identifier] Type [ArithExpr] | 
            AttrStmt [Identifier] [ArithExpr] deriving (Show, Eq)

data ArithUnOp =    MinusUnOp | 
                    PlusUnOp 
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


data Term = LitTerm Literal | 
            IdTerm Identifier |
            SubcallTerm SubprogCall 
            deriving (Show, Eq)

data ArithExpr =    ArithUnExpr ArithUnOp ArithExpr | 
                    ArithBinExpr ArithBinOp ArithExpr ArithExpr | 
                    ArithTerm Term
                    deriving (Show, Eq)

data IdentList = IdentList [Identifier] deriving (Show, Eq)

