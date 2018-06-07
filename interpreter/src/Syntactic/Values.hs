module Syntactic.Values where

import qualified Data.Map as M
import qualified Execution.Graph as G
import Syntactic.Types

{- Values for all data types -}
data Value =    Integer Integer | 
                Float Double | 
                Char Char | 
                String String | 
                Bool Bool | 
                List [Value] | 
                EmptyList |
                Pair (Value, Value) | 
                Triple (Value, Value, Value) | 
                Quadruple (Value, Value, Value, Value) | 
                Map (M.Map Value  Value) |
                EmptyMap |
                Graph (G.Graph Value Value) |
                Setter (M.Map String Value)


instance Eq Value where
    (==) (Integer i) (Integer i2)       = i == i2 
    (==) (Float f) (Float f2)           = f == f2 
    (==) (Float f) (Integer i)          = f == fromInteger i  
    (==) (Integer i) (Float f)          = fromInteger i == f
    (==) (Char c) (Char c2)             = c == c2 
    (==) (String s) (String s2)         = s == s2 
    (==) (Bool b) (Bool b2)             = b == b2 
    (==) (List l1) (List l2)            = l1 == l2 
    (==) (EmptyList) (EmptyList)        = True 
    (==) (Pair p1) (Pair p2)            = p1 == p2 
    (==) (Triple t1) (Triple t2)        = t1 == t2 
    (==) (Quadruple q1) (Quadruple q2)  = q1 == q2 
    (==) (Map m1) (Map m2)              = m1 == m2
    (==) (EmptyMap) (EmptyMap)          = True 
    (==) (Graph g1) (Graph g2)          = g1 == g2
    (==) (Setter s1) (Setter s2)        = s1 == s2

    (/=) (Integer i) (Integer i2)       = i /= i2 
    (/=) (Float f) (Float f2)           = f /= f2 
    (/=) (Float f) (Integer i)          = f /= fromInteger i  
    (/=) (Integer i) (Float f)          = fromInteger i /= f
    (/=) (Char c) (Char c2)             = c /= c2 
    (/=) (String s) (String s2)         = s /= s2 
    (/=) (Bool b) (Bool b2)             = b /= b2 
    (/=) (List l1) (List l2)            = l1 /= l2 
    (/=) (EmptyList) (EmptyList)        = False 
    (/=) (Pair p1) (Pair p2)            = p1 /= p2 
    (/=) (Triple t1) (Triple t2)        = t1 /= t2 
    (/=) (Quadruple q1) (Quadruple q2)  = q1 /= q2 
    (/=) (Map m1) (Map m2)              = m1 /= m2
    (/=) (EmptyMap) (EmptyMap)          = False 
    (/=) (Graph g1) (Graph g2)          = g1 /= g2
    (/=) (Setter s1) (Setter s2)        = s1 /= s2

instance Ord Value where
    (<=) (Integer i) (Integer i2)       = i <= i2 
    (<=) (Float f) (Float f2)           = f <= f2 
    (<=) (Float f) (Integer i)          = f <= fromInteger i  
    (<=) (Integer i) (Float f)          = fromInteger i <= f
    (<=) (Char c) (Char c2)             = c <= c2 
    (<=) (String s) (String s2)         = s <= s2 
    (<=) (Bool b) (Bool b2)             = b <= b2 
    (<=) (List l1) (List l2)            = l1 <= l2 
    (<=) (EmptyList) (EmptyList)        = EmptyList <=  EmptyList 
    (<=) (Pair p1) (Pair p2)            = p1 <= p2 
    (<=) (Triple t1) (Triple t2)        = t1 <= t2 
    (<=) (Quadruple q1) (Quadruple q2)  = q1 <= q2 
    (<=) (Map m1) (Map m2)              = m1 <= m2
    (<=) (EmptyMap) (EmptyMap)          = EmptyMap <= EmptyMap
    (<=) (Graph g1) (Graph g2)          = g1 <= g2
    (<=) (Setter s1) (Setter s2)        = s1 <= s2

    (<) (Integer i) (Integer i2)       = i < i2 
    (<) (Float f) (Float f2)           = f < f2 
    (<) (Float f) (Integer i)          = f < fromInteger i  
    (<) (Integer i) (Float f)          = fromInteger i < f
    (<) (Char c) (Char c2)             = c < c2 
    (<) (String s) (String s2)         = s < s2 
    (<) (Bool b) (Bool b2)             = b < b2 
    (<) (List l1) (List l2)            = l1 < l2 
    (<) (EmptyList) (EmptyList)        = EmptyList <  EmptyList 
    (<) (Pair p1) (Pair p2)            = p1 < p2 
    (<) (Triple t1) (Triple t2)        = t1 < t2 
    (<) (Quadruple q1) (Quadruple q2)  = q1 < q2 
    (<) (Map m1) (Map m2)              = m1 < m2
    (<) (EmptyMap) (EmptyMap)          = EmptyMap < EmptyMap
    (<) (Graph g1) (Graph g2)          = g1 < g2
    (<) (Setter s1) (Setter s2)        = s1 < s2

    (>) (Integer i) (Integer i2)       = i > i2 
    (>) (Float f) (Float f2)           = f > f2 
    (>) (Float f) (Integer i)          = f > fromInteger i  
    (>) (Integer i) (Float f)          = fromInteger i > f
    (>) (Char c) (Char c2)             = c > c2 
    (>) (String s) (String s2)         = s > s2 
    (>) (Bool b) (Bool b2)             = b > b2 
    (>) (List l1) (List l2)            = l1 > l2 
    (>) (EmptyList) (EmptyList)        = EmptyList >  EmptyList 
    (>) (Pair p1) (Pair p2)            = p1 > p2 
    (>) (Triple t1) (Triple t2)        = t1 > t2 
    (>) (Quadruple q1) (Quadruple q2)  = q1 > q2 
    (>) (Map m1) (Map m2)              = m1 > m2
    (>) (EmptyMap) (EmptyMap)          = EmptyMap > EmptyMap
    (>) (Graph g1) (Graph g2)          = g1 > g2
    (>) (Setter s1) (Setter s2)        = s1 > s2

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
    show (Setter x)    = show x
    show EmptyList     = "[]"


