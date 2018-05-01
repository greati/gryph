module Parsing.Parser where

import Lexer
import Text.ParserCombinators.Parsec
import GTok
import GphTokens

type Identifier = String
type Type = String
data Stmt = ReadStmt Identifier | PrintStmt Identifier | DeclStmt Identifier Type deriving (Show, Eq)


gryphParser :: GenParser GphTokenPos st [Stmt]
gryphParser = 
    do result <- many stmt
       return result

stmt :: GenParser GphTokenPos st Stmt
stmt = readStmt 
    <|> printStmt
    <|> declStmt


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

parseFile :: String -> IO [Stmt]
parseFile file = 
    do program <- readFile file
       case parse gryphParser "" (alexScanTokens program) of
            Left e  -> print e >> fail "parse error"
            Right r -> return r

