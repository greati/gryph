module Execution.Graph where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Tuple      as T


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

instance Eq (Edge a b) where
    (==) (Edge x1 y1 _) (Edge x2 y2 _) = (x1 == y1) && (x2 == y2)

instance (Show b, Show a) => Show (Edge a b) where
    show (Edge v1 v2 p) = show v1 ++ " -> " ++ show v2 ++ " [" ++ show p ++ "]"

-- |Graph with vertices indexed by integers holding data of type a, and edges holding data of type b
data Graph a b = Graph (S.Set (Vertex a)) (M.Map Int [Edge a b]) deriving Eq

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

fromEdges' :: [Edge a b] -> ([Vertex a], [(Int, [Edge a b])])
fromEdges' []                                                      = ([],[])
fromEdges' [ed@(Edge vx@(Vertex idx x) vy@(Vertex idy y) _)]       = (vx : vy : [], (idx, [ed]) : [] )
fromEdges' (ed@(Edge vx@(Vertex idx x) vy@(Vertex idy y) _) : eds) = (vx : vy : T.fst (fromEdges' eds), fromEdges'' idx ed (T.snd (fromEdges' eds)))

fromEdges'' :: Int -> Edge a b -> [(Int, [Edge a b])] -> [(Int, [Edge a b])]
fromEdges'' n ed []     = (n, [ed]) : []
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

connect :: Graph a b -> Vertex a -> Vertex a -> b -> Graph a b 
connect g@(Graph vs es) v1@(Vertex x1 p1) v2@(Vertex x2 p2) p
    | isVertexPresent g v1 && isVertexPresent g v2 = Graph vs (M.adjust (\xs-> (edgeFromVertexes v1 v2 p):xs) x1 es)
    | otherwise = error "both vertices must be present"


-- |Insert an edge. Error if vertices aren't in the list
insertEdge :: Graph a b -> Edge a b -> Graph a b
insertEdge (Graph vs es) ed@(Edge v1@(Vertex id _) v2 p)
    | not (elem v1 vs) || not(elem v2 vs) = error "both vertices must be present"
    | otherwise = Graph vs (M.insertWith (++) id [ed] es)

-- |Update an edge. Error if vertices aren't in the list
updateEdge :: Graph a b -> Edge a b -> Graph a b
updateEdge (Graph vs es) ed@(Edge v1@(Vertex id _) v2 p)
    | not (elem v1 vs) || not(elem v2 vs) = error "both vertices must be present"
    | otherwise = Graph vs (updateEdge' es id ed)
    where
        updateEdge' es id ed 
            | not (M.member id es) = error "the edge must have exist"
            | otherwise            = updateEdge'' id ed (es M.! id)
        updateEdge'' id ed  [x]    = M.fromList [(id , [ed])] 
        updateEdge'' id ed   xs    = M.fromList [(id, updateEdge''' ed xs)]
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
            | not (M.member id es) = error "the edge must have exist"
            | otherwise            = deleteEdge'' id ed (es M.! id)
        deleteEdge'' id ed  [x]    = M.fromList [] 
        deleteEdge'' id ed   xs    = M.fromList [(id, deleteEdge''' ed xs)]
        deleteEdge''' ed    [x]    = []
        deleteEdge''' ed@(Edge v1 v2 _) (x@(Edge x1 x2 _) : xs)
            | v1 == x1 && v2 == x2 = xs
            | otherwise            = x : deleteEdge''' ed xs   
