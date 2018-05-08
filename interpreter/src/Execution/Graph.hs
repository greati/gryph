module Execution.Graph where

import qualified Data.Map as M
import qualified Data.Set as S


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
insertEdge (Graph vs es) (Edge v1 v2 p) = undefined



