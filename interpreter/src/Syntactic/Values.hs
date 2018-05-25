module Syntactic.Values where

import qualified Data.Map as M
import qualified Execution.Graph as G

{- Values for all data types -}
data Value =    Integer Integer | 
                Float Double | 
                Char Char | 
                String String | 
                Bool Bool | 
                List [Value] | 
                Pair (Value, Value) | 
                Triple (Value, Value, Value) | 
                Quadruple (Value, Value, Value, Value) | 
                Map (M.Map Value  Value) |
		Graph (G.Graph Value Value) 
                deriving (Eq, Ord)

instance Show Value where
    show (Integer x)   = show x
    show (Float x)     = show x
    show (Char x)      = show x
    show (String x)    = show x
    show (Bool x)      = show x
    show (List x)      = show x
    show (Pair x)      = show x
    show (Triple x)    = show x
    show (Quadruple x) = show x
    show (Map x)       = show x
    show (Graph x)     = show x
