module Syntactic.GphTokens where

{-| Tokens of the Gryph language -}
data GphToken =
    GTokIdentifier String           |
    GTokSemicolon                   |
    GTokFunction                    |
    GTokProc                        |
    GTokIf                          |
    GTokElse                        |
    GTokFor                         |
    GTokWhile                       |
    GTokReturn                      |
    GTokRead                        |
    GTokPrint                       |
    GTokWhere                       |
    GTokWhen                        |
    GTokOver                        |   
    GTokDFS                         |
    GTokBFS                         |
    GTokIntLit String               |
    GTokFloatLit String             |
    GTokCharLit  String             |
    GTokStringLit String            |
    GTokBoolLit String              |
    GTokEdgeSym String              |
    GTokLogicalOp String            |
    GTokRangeOp                     |
    GTokType String                 |
    GTokAt                          |
    GTokModulus                     |
    GTokAssignment                  |
    GTokPlus                        |
    GTokPlusPlus                    |
    GTokHat                         |
    GTokMinus                       |
    GTokTimes                       |  
    GTokTimesTimes                  |  
    GTokDivision                    |
    GTokComma                       |
    GTokColon                       |
    GTokEq                          |
    GTokNeq                         |
    GTokLess                        |
    GTokLessEq                      |
    GTokGreater                     |
    GTokGreaterEq                   |
    GTokLParen                      |
    GTokRParen                      |
    GTokLCurly                      |
    GTokRCurly                      |
    GTokLSquare                     |
    GTokRSquare                     |
    GTokQuestion                    |
    GTokPipe                        |
    GTokDot                         |
    GTokUndefined String            |
    GTokError String
    deriving (Show, Eq) 
