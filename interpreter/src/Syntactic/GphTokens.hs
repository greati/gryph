module GphTokens where

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
    GTokMinus                       |
    GTokTimes                       |  
    GTokDivision                    |
    GTokComma                       |
    GTokColon                       |
    GTokLess                        |
    GTokGreater                     |
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
