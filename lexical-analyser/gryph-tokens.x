{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$varname = [$alpha $digit \_]
 
tokens :-
    $white+                                 ;                           
    "#".*                                  ;
    $alpha $varname*[\']+$varname+      {\p s -> Error}           
    $alpha $varname*[\']*               {\p s -> Var p s}           
    .                                   {\p s -> Undefined}
{

data Token = 
    Var AlexPosn String     |
    Undefined |
    Error 
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
