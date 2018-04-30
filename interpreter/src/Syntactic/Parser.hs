module Parsing.Parser where

import Lexer
import Text.ParserCombinators.Parsec


type Identifier = String
data Type = Int | Float
data Stmt = ReadStmt Identifier | PrintStmt Identifier | DeclStmt Identifier Type


gryphParser :: GenParser Token st [Stmt]
gryphParser = 
    do result <- many stmt
       return result

stmt :: GenParser Token st Stmt
stmt = readStmt 
    <|> printStmt
    <|> declStmt


readStmt :: GenParser Token st Stmt
readStmt = undefined

printStmt :: GenParser Token st Stmt
printStmt = undefined

declStmt :: GenParser Token st Stmt
declStmt = undefined
