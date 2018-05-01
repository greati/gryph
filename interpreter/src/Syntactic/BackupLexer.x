{
module Lexer (main, Token(..), AlexPosn(..), alexScanTokens) where

import GphTokens
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$varname = [$alpha $digit \_]
@boolvalues = true|false -- boolean values
@type = int|float|char|string|bool
@logical_ops = \=\=|and|or|\!\=|xor|not|\>\=|\<\=
@edges = \-\-|\-\>|\<\-

tokens :-
    $white+                             ;                           
    "#".*                               ;
    ";"                                 {\p s -> SemiColon p s}
    \'.\'                               {\p s -> CharLit p s}
    \" [^\"]* \"                        {\p s -> StringLit p s}
    $digit+\.$digit+                    {\p s -> FloatLit p s}
    \.\.                                {\p s -> RangeOp p s}
    $digit+                             {\p s -> IntLit p s}
    @boolvalues                         {\p s -> BoolLit p s}
    @edges                              {\p s -> EdgeSym p s}
    @type                               {\p s -> Type p s}
    if                                  {\p s -> If p s}
    else                                {\p s -> Else p s}
    for                                 {\p s -> For p s}
    while                               {\p s -> While p s}
    where                               {\p s -> Where p s}
    when                                {\p s -> When p s}
    over                                {\p s -> Over p s}
    fun                                 {\p s -> Function p s}
    proc                                {\p s -> Procedure p s}
    dfs                                 {\p s -> DFS p s}
    bfs                                 {\p s -> BFS p s}
    return                              {\p s -> Return p s}
    print                               {\p s -> Print p s}
    read                                {\p s -> Read p s}
    $alpha $varname*[\']+[^\'$white]+   {\p s -> Error p s}           
    $alpha $varname*[\']*               {\p s -> Var p s }
    @logical_ops                        {\p s -> LogicalOps p s}
    \@                                  {\p s -> AtOp p s}
    \%                                  {\p s -> ModulusOp p s}
    \=                                  {\p s -> AssignmentOp p s}
    \+                                  {\p s -> PlusOp p s}
    \-                                  {\p s -> MinusOp p s}
    \*                                  {\p s -> TimesOp p s}
    \/                                  {\p s -> DivisionOp p s}
    \(                                  {\p s -> OpenParent p s}
    \)                                  {\p s -> CloseParent p s}
    \{                                  {\p s -> OpenCurly p s}
    \}                                  {\p s -> CloseCurly p s}
    \[                                  {\p s -> OpenSquare p s}
    \]                                  {\p s -> CloseSquare p s}
    \:                                  {\p s -> Colon p s}
    \<                                  {\p s -> Less p s}
    \>                                  {\p s -> Greater p s}
    \,                                  {\p s -> Comma p s}
    \?                                  {\p s -> QuestionMark p s}
    \|                                  {\p s -> Pipe p s}
    \.                                  {\p s -> Dot p s}
    .                                   {\p s -> Undefined p s}
{

type GphTokenPos = (GphToken, AlexPosn)

data Token = 
    SemiColon AlexPosn String                 |
    Var AlexPosn String                 |
    Function AlexPosn String                  |
    Procedure AlexPosn String                                   |
    If AlexPosn  String                                         |
    Else AlexPosn  String                                       |
    For AlexPosn  String                                        |
    While AlexPosn  String                                      |
    Return AlexPosn  String                                     |
    Read AlexPosn   String                                      |
    Print AlexPosn  String                                      |
    Where AlexPosn  String                                      |
    When AlexPosn String                                        |
    Over AlexPosn String                                        |
    DFS AlexPosn String                                         |
    BFS AlexPosn  String                                        |
    Sym AlexPosn String                 |
    IntLit AlexPosn String              |
    FloatLit AlexPosn String            |
    CharLit AlexPosn String             |
    StringLit AlexPosn String           |
    BoolLit AlexPosn String             |
    EdgeSym AlexPosn String             |
    LogicalOps AlexPosn String          |
    RangeOp AlexPosn  String                                    |
    Type AlexPosn String                |
    AtOp AlexPosn  String                                       |
    ModulusOp AlexPosn  String                                  |
    AssignmentOp AlexPosn  String                               |
    PlusOp AlexPosn  String                                     |
    MinusOp AlexPosn  String                                    |
    TimesOp AlexPosn  String                                    |
    DivisionOp AlexPosn  String                                 |
    Comma AlexPosn  String                                      |
    Colon AlexPosn  String                                      |
    Less AlexPosn  String                                       |
    Greater AlexPosn  String                                    |
    OpenParent AlexPosn  String                                 |
    CloseParent AlexPosn  String                                |
    OpenCurly AlexPosn  String                                  |
    CloseCurly AlexPosn  String                                 |
    OpenSquare AlexPosn  String                                 |
    CloseSquare AlexPosn  String                                |
    QuestionMark AlexPosn  String                               |
    Pipe AlexPosn  String                                       |
    Dot AlexPosn  String                                        |
    Undefined  AlexPosn String                                          |
    Error AlexPosn String                  
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
