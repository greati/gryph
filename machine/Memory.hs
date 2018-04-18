module Memory where

import Data.Map (Map)

{- Language values -}
data Primitive          = Integer Integer | Float Double | Char Char | String String | Bool Bool deriving (Show)
type List a             = [a]               
type Pair a b           = (a, b)
type Triple a b c       = (a, b, c)
type Quadruple a b c d  = (a, b, c, d)
type Dictionary a b     = Map a b

{- Language memory implementation -}
type Name   = String
type Scope  = String

type Identifier     = (Name, Scope)

type PrimitiveCell  = ([Primitive])

type Memory         = Map Identifier Cell

