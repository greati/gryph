{
module Syntactic.Lexer (main, GphTokenPos(..), AlexPosn(..), alexScanTokens) where

import Syntactic.GphTokens
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$varname = [$alpha $digit \_]
@type = int|float|char|string|bool
@edges = \-\-|\-\>|\<\-

tokens :-
    $white+                             ;                           
    "#".*                               ;
    ";"                                 {\p s -> (GTokSemicolon, p)}
    \'.\'                               {\p s -> (GTokCharLit s, p)}
    \" [^\"]* \"                        {\p s -> (GTokStringLit s, p)}
    $digit+\.$digit+                    {\p s -> (GTokFloatLit s, p)}
    \.\.                                {\p s -> (GTokRangeOp, p)}
    $digit+                             {\p s -> (GTokIntLit s, p)}
    \-\>                                {\p s -> (GTokRightEdge, p)}
    \<\-                                {\p s -> (GTokLeftEdge, p)}
    \-\-                                {\p s -> (GTokDoubleEdge, p)}
    \&                                  {\p s -> (GTokAmpersand, p)}
    @type                               {\p s -> (GTokType s, p)}
    true                                {\p s -> (GTokTrue, p)}
    false                               {\p s -> (GTokFalse, p)}
    and                                 {\p s -> (GTokAnd, p)}
    or                                  {\p s -> (GTokOr, p)}
    not                                 {\p s -> (GTokNot, p)}
    xor                                 {\p s -> (GTokXor, p)}
    if                                  {\p s -> (GTokIf, p)}
    else                                {\p s -> (GTokElse, p)}
    for                                 {\p s -> (GTokFor, p)}
    while                               {\p s -> (GTokWhile, p)}
    in                                  {\p s -> (GTokIn, p)}
    where                               {\p s -> (GTokWhere, p)}
    when                                {\p s -> (GTokWhen, p)}
    over                                {\p s -> (GTokOver, p)}
    sub                                 {\p s -> (GTokSub, p)}
    dfs                                 {\p s -> (GTokDFS, p)}
    bfs                                 {\p s -> (GTokBFS, p)}
    from                                {\p s -> (GTokFrom, p)}
    return                              {\p s -> (GTokReturn, p)}
    print                               {\p s -> (GTokPrint, p)}
    read                                {\p s -> (GTokRead, p)}
    $alpha $varname*[\']+[^\'$white]+   {\p s -> (GTokError s, p)}           
    $alpha $varname*[\']*               {\p s -> (GTokIdentifier s, p)}
    \@                                  {\p s -> (GTokAt, p)}
    \^                                  {\p s -> (GTokHat, p)}
    \%                                  {\p s -> (GTokModulus, p)}
    \+\+                                {\p s -> (GTokPlusPlus, p)}
    \*\*                                {\p s -> (GTokTimesTimes, p)}
    \!\=                                {\p s -> (GTokNeq, p)}
    \=\=                                {\p s -> (GTokEq, p)}
    \=                                  {\p s -> (GTokAssignment, p)}
    \+                                  {\p s -> (GTokPlus, p)}
    \-                                  {\p s -> (GTokMinus, p)}
    \*                                  {\p s -> (GTokTimes, p)}
    \/                                  {\p s -> (GTokDivision, p)}
    \(                                  {\p s -> (GTokLParen, p)}
    \)                                  {\p s -> (GTokRParen, p)}
    \{                                  {\p s -> (GTokLCurly, p)}
    \}                                  {\p s -> (GTokRCurly, p)}
    \[                                  {\p s -> (GTokLSquare, p)}
    \]                                  {\p s -> (GTokRSquare, p)}
    \:                                  {\p s -> (GTokColon, p)}
    \<\=                                {\p s -> (GTokLessEq, p)}
    \<                                  {\p s -> (GTokLess, p)}
    \>\=                                {\p s -> (GTokGreaterEq, p)}
    \>                                  {\p s -> (GTokGreater, p)}
    \,                                  {\p s -> (GTokComma, p)}
    \?                                  {\p s -> (GTokQuestion, p)}
    \|                                  {\p s -> (GTokPipe, p)}
    \.                                  {\p s -> (GTokDot, p)}
    .                                   {\p s -> (GTokUndefined s, p)}
{

type GphTokenPos = (GphToken, AlexPosn)

main = do
    s <- getContents
    print (alexScanTokens s)
}
