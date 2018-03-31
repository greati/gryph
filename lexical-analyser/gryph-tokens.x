{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$varname = [$alpha $digit \_]
@boolvalues = true|false -- boolean values
@type = int|double|char|string|graph|bool
@logical_ops = \=\=|and|or|\!\=|xor|not|\>\=|\<\=
@edges = \-\-|\-\>|\<\-

tokens :-
    $white+                             ;                           
    "#".*                               ;
    ";"                                 {\p s -> SemiColon p}
    \'.\'                               {\p s -> CharLit p s}
    \" [^\"]* \"                        {\p s -> StringLit p s}
    $digit+\.$digit+                    {\p s -> FloatLit p s}
    \.\.                                {\p s -> RangeOp p}
    $digit+                             {\p s -> IntLit p s}
    @boolvalues                         {\p s -> BoolLit p s}
    @edges                              {\p s -> EdgeSym p s}
    @type                               {\p s -> Type p s}
    if                                  {\p s -> If p}
    else                                {\p s -> Else p}
    for                                 {\p s -> For p}
    while                               {\p s -> While p}
    where                               {\p s -> Where p}
    when                                {\p s -> When p}
    over                                {\p s -> Over p}
    fun                                 {\p s -> Function p}
    proc                                {\p s -> Procedure p}
    dfs                                 {\p s -> DFS p}
    bfs                                 {\p s -> BFS p}
    return                              {\p s -> Return p}
    $alpha $varname*[\']+[^\']+         {\p s -> Error}           
    $alpha $varname*[\']*               {\p s -> Var p s}
    @logical_ops                        {\p s -> LogicalOps p s}
    [\%\=\+\-\*\/\(\)\[\]\{\}\:\<\>\,]  {\p s -> Sym p s}
    .                                   {\p s -> Undefined}
{

data Token = 
    SemiColon AlexPosn                  |
    Var AlexPosn String                 |
    Function AlexPosn                   |
    Procedure AlexPosn                  |
    If AlexPosn                         |
    Else AlexPosn                       |
    For AlexPosn                        |
    While AlexPosn                      |
    Return AlexPosn                     |
    Where AlexPosn                      |
    When AlexPosn                       |
    Over AlexPosn                       |
    DFS AlexPosn                        |
    BFS AlexPosn                        |
    Sym AlexPosn String                 |
    IntLit AlexPosn String              |
    FloatLit AlexPosn String            |
    CharLit AlexPosn String             |
    StringLit AlexPosn String           |
    BoolLit AlexPosn String             |
    EdgeSym AlexPosn String             |
    LogicalOps AlexPosn String          |
    RangeOp AlexPosn                    |
    Type AlexPosn String                |
    Undefined                           |
    Error 
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
