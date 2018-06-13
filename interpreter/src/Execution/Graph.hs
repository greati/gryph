module Execution.Graph where

import qualified Data.Map.Strict   as M
import qualified Data.Set          as S
import qualified Data.Tuple        as T
import Data.List                   as L

-- |Vertex indexed by integer values, holding data of type a
data Vertex a = Vertex Int a

instance (Show a) => Show (Vertex a) where
    show (Vertex x p) = show x ++ "(" ++ show p ++ ")"

instance Eq (Vertex a) where
    (==) (Vertex x _) (Vertex y _) = (x == y)

instance Ord (Vertex a) where
    compare (Vertex x1 _) (Vertex x2 _) = compare x1 x2        

-- |Edge with vertices storing values of type a, holding data of type b
data Edge a b = Edge (Vertex a) (Vertex a) b
                deriving (Ord)                
instance Eq (Edge a b) where
    (==) (Edge x1 y1 _) (Edge x2 y2 _) = (x1 == y1) && (x2 == y2)

instance (Show b, Show a) => Show (Edge a b) where
    show (Edge v1 v2 p) = show v1 ++ " -> " ++ show v2 ++ " [" ++ show p ++ "]"

-- |Graph with vertices indexed by integers holding data of type a, and edges holding data of type b
data Graph a b = Graph (S.Set (Vertex a)) (M.Map Int [Edge a b]) deriving (Eq, Ord)
instance (Show a, Show b) => Show (Graph a b) where
    show (Graph vs es) = show vs ++ "\n\n" ++ show es 

-- For debugging
g = fromVertices [1#2]

-- |Create a graph from a list of vertices, with no edges
fromVertices :: [Vertex a] -> Graph a b
fromVertices vs = Graph (S.fromList vs) M.empty

-- |Create a graph from a list of edges, with no vertices
fromEdges :: [Edge a b] -> Graph a b
fromEdges es = Graph (S.fromList vertices) (M.fromList edges)
    where
        (vertices, edges) = fromEdges' es
        
        fromEdges' []                                                      = ([],[])
        fromEdges' [ed@(Edge vx@(Vertex idx x) vy@(Vertex idy y) _)]       = (vx : vy : [], (idx, [ed]) : [] )
        fromEdges' (ed@(Edge vx@(Vertex idx x) vy@(Vertex idy y) _) : eds) = (vx : vy : T.fst (fromEdges' eds), fromEdges'' idx ed (T.snd (fromEdges' eds)))
        
        fromEdges'' n ed [] = (n, [ed]) : []
        fromEdges'' n ed (e'@(id,e):es) 
            | n == id   = (id, ed : e) : es
            | otherwise = e' : fromEdges'' n ed es  

-- |Create an edge from a tuple of its components
edgeFromTuple :: (Vertex a, Vertex a, b) -> Edge a b
edgeFromTuple (x, y, p) = Edge x y p

edgeFromVertexes :: (Vertex a) -> (Vertex a) -> b -> Edge a b
edgeFromVertexes v1 v2 p = Edge v1 v2 p

-- |Create a vertex from its Id and Payload
makeVertex :: Int -> a -> Vertex a
makeVertex x p = Vertex x p

(#) = makeVertex

-- |Returns the Id of a vertex
vertexId :: Vertex a -> Int
vertexId (Vertex x _) = x

-- |Insert a vertex
insertVertex :: Graph a b -> Vertex a -> Graph a b
insertVertex (Graph vs p) v = Graph (S.insert v vs) p

isVertexPresent :: Graph a b -> Vertex a -> Bool
isVertexPresent (Graph vs es) v'@(Vertex v _) = S.member v' vs

isEdgePresent :: Graph a b -> Edge a b -> Bool
isEdgePresent (Graph vc es) (Edge v1@(Vertex id _) v2 _) = isEdgePresent' (es M.!? id) v2
    where
        isEdgePresent' Nothing _                     = False
        isEdgePresent' (Just []) _                   = False
        isEdgePresent' (Just (Edge _ v2' _ : xs)) v2 = v2 == v2' || isEdgePresent' (Just xs) v2

connect :: Graph a b -> Vertex a -> Vertex a -> b -> Graph a b 
connect g@(Graph vs es) v1@(Vertex x1 p1) v2@(Vertex x2 p2) p
    | isVertexPresent g v1 && isVertexPresent g v2 = Graph vs (M.adjust (\xs-> (edgeFromVertexes v1 v2 p):xs) x1 es)
    | otherwise = error "both vertices must be present"


-- |Insert an edge. Error if vertices aren't in the list
insertEdge :: Graph a b -> Edge a b -> Graph a b
insertEdge g@(Graph vs es) ed@(Edge v1@(Vertex id _) v2 p) = 
    if not (elem v1 vs) || not(elem v2 vs) 
    then error "both vertices must be present"
    else 
        case (list es id) of
            Nothing -> Graph vs (M.insertWith (++) id [ed] es)
            Just l -> if isIn ed l
                      then updateEdge g ed
                      else Graph vs (M.insertWith (++) id [ed] es)
    where
        list es id = (es M.!? id)
        isIn ed []     = False
        isIn ed@(Edge v1@(Vertex id1 _) v2@(Vertex id2 _) _) ( (Edge (Vertex idx _) (Vertex idy _) _) : xs) = if id1 == idx && id2 == idy then True else isIn ed xs

-- |Update an edge. Error if vertices aren't in the list
updateEdge :: Graph a b -> Edge a b -> Graph a b
updateEdge (Graph vs es) ed@(Edge v1@(Vertex id _) v2 p)
    | not (elem v1 vs) || not(elem v2 vs) = error "both vertices must be present"
    | otherwise = Graph vs (updateEdge' es id ed)
    where
        updateEdge' es id ed 
            | not (M.member id es) = error "the edge must have exist"
            | otherwise            = updateEdge'' id ed (es M.! id)
        updateEdge'' id ed  [x]    = (M.insert id [ed] es) 
        updateEdge'' id ed   xs    = (M.insert id (updateEdge''' ed xs) es)
        updateEdge''' ed    [x]    = [ed]
        updateEdge''' ed@(Edge v1 v2 _) (x@(Edge x1 x2 _) : xs)
            | v1 == x1 && v2 == x2 = (ed : xs)
            | otherwise            = x : updateEdge''' ed xs

-- |Delete an edge. Error if the vertices aren't in the list
deleteEdge :: Graph a b -> Edge a b -> Graph a b
deleteEdge (Graph vs es) ed@(Edge v1@(Vertex id _) v2 p)
    | not (elem v1 vs) || not(elem v2 vs) = error "both vertices must be present"
    | otherwise = Graph vs (deleteEdge' es id ed)
    where
        deleteEdge' es id ed 
            | not (M.member id es) = error "1the edge must have exist"
            | otherwise            = deleteEdge'' id ed (es M.! id)
        deleteEdge'' id ed@(Edge v1 v2 _)  [Edge v1' v2' _] = 
                                    if v1 == v1' && v2 == v2' 
                                    then M.delete id es
                                    else error "2The edge must have exist"
        deleteEdge'' id ed   xs    = M.insert id (deleteEdge''' ed xs) es
        deleteEdge''' ed@(Edge v1 v2 _)  [Edge v1' v2' _] = 
                                    if v1 == v1' && v2 == v2' 
                                    then []
                                    else error "3The edge must have exist"
        deleteEdge''' ed@(Edge v1 v2 _) (x@(Edge v1' v2' _) : xs)
            | v1 == v1' && v2 == v2' = xs
            | otherwise              = x : deleteEdge''' ed xs   

-- |Get edges of a vertex
getEdges :: Graph a b -> Vertex a -> [Edge a b]
getEdges (Graph vs es) v@(Vertex n _) 
    | not (elem v vs) = error "the vertex must be present"
    | not (M.member n es) = []
    | otherwise       = es M.! n
                
-- |Get the vertices of a graph
getVertices :: Graph a b -> [Vertex a]
getVertices (Graph vs _) = S.toList vs

-- |Delete a vertex of a graph
deleteVertex :: Vertex a -> Graph a b -> Graph a b
deleteVertex = undefined

-- | Generate list of vertices
fromListToVertices :: [(Int,a)] -> [Vertex a]
fromListToVertices []         = []
fromListToVertices ((n,x):xs) = (Vertex n x) : fromListToVertices xs

-- | Get the index of vertex or next
getVertexFromValue :: Eq a => (Graph a b) -> a -> Bool -> (Vertex a)
getVertexFromValue (Graph vs _) v next = getVertexFromValue' (S.toList vs) v 0 next
    where
        getVertexFromValue' [] v n False = Vertex (-1) v
        getVertexFromValue' [] v n True = Vertex (n + 1) v
        getVertexFromValue' ( v'@(Vertex i value) : xs ) v n next
            | v == value = v'
            | otherwise  = getVertexFromValue' xs v (max i n) next 
