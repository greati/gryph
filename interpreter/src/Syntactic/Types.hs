module Syntactic.Types where

data GType =    GInteger        |
                GFloat          |
                GString         |
                GChar           |
                GBool           |
                GEmpty |
                GList GType     |
                GPair GType GType   |
                GTriple GType GType GType  |
                GQuadruple GType GType GType GType |
                GDict GType GType |
                GGraphVertex GType |
                GGraphVertexEdge GType GType |
                GUserType String |
                GAnonymousStruct
                deriving (Show, Eq, Ord)


