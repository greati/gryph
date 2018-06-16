module Syntactic.Types where

data GType =    GInteger                            |
                GFloat                              |
                GString                             |
                GChar                               |
                GBool                               |
                GListEmpty                          |
                GDictEmpty                          |
                GList GType                         |
                GPair GType GType                   |
                GTriple GType GType GType           |
                GQuadruple GType GType GType GType  |
                GDict GType GType                   |
                GGraphEmpty                         |
                GEdgeEmpty                          |
                GGraphVertexEdge GType GType        |
                GUserType String                    |
                GAnonymousStruct
                deriving (Show, Eq, Ord)

-- | Check types for compatibility
checkCompatType :: GType -> GType -> Bool
checkCompatType t t' = if t == t' then True
                    else case (t,t') of
                        (GFloat, GInteger)  -> True
                        _                   -> checkCompatType' t t'

checkCompatType' :: GType -> GType -> Bool
checkCompatType' t t' = if t == t' then True
                     else case (t,t') of
                        (GGraphVertexEdge v1 _, GGraphVertexEdge v2 GEdgeEmpty) -> checkCompatType' v1 v2--(v1 == v2)
                        (GGraphVertexEdge v2 GEdgeEmpty, GGraphVertexEdge v1 _) -> checkCompatType' v1 v2--(v1 == v2)(v1 == v2)
                        (GGraphVertexEdge _ _, GGraphEmpty) -> True
                        (GGraphEmpty, GGraphVertexEdge _ _) -> True
                        (GGraphVertexEdge v1 e1, GGraphVertexEdge v2 e2) -> checkCompatType' e1 e2 && checkCompatType' v1 v2--(e1 == e2) && (v1 == v2)
                        (GList _, GListEmpty)     -> True
                        (GListEmpty ,GList _)     -> True
                        (GDict _ _, GDictEmpty)   -> True
                        (GDictEmpty, GDict _ _ )  -> True
                        (GDict k1 v1, GDict k2 v2)  -> checkCompatType' k1 k2 && checkCompatType' v1 v2--(e1 == e2) && (v1 == v2)
                        (GList l, GList l') -> checkCompatType' l l'
                        --(GList l, GList l') -> checkCompatType (GList l) l'
                        --(GList l, GList l') -> checkCompatType (l) $ GList l'
                        (GUserType _, GAnonymousStruct) -> True
                        --(GAnonymousStruct, GUserType _) -> True
                        (GPair p1 p2, GPair p1' p2') -> (checkCompatType' p1 p1') && (checkCompatType' p2 p2')
                        (GTriple p1 p2 p3, GTriple p1' p2' p3') -> (checkCompatType' p1 p1') && (checkCompatType' p2 p2') && (checkCompatType' p3 p3')
                        (GQuadruple p1 p2 p3 p4, GQuadruple p1' p2' p3' p4') -> (checkCompatType' p1 p1') && (checkCompatType' p2 p2') && (checkCompatType' p3 p3') && (checkCompatType' p4 p4')
                        _                   -> False


