module Graph where

import qualified Data.Map as M
import qualified Data.Set as S

-- |Payload for vertices and edges
data Payload a = Payload a deriving (Show)

-- |Vertex indexed by integer values, holding data of type a
data Vertex a = Vertex Int (Payload a) deriving (Show)

instance Eq (Vertex a) where
    (==) (Vertex x _) (Vertex y _) = (x == y)

instance Ord (Vertex a) where
    compare (Vertex x1 _) (Vertex x2 _) = compare x1 x2        

-- |Edge with vertices storing values of type a, holding data of type b
data Edge a b = Edge (Vertex a) (Vertex a) (Payload b) deriving (Show)

instance Eq (Edge a b) where
    (==) (Edge x1 y1 _) (Edge x2 y2 _) = (x1 == y1) && (x2 == y2)

-- |Graph with vertices indexed by integers holding data of type a, and edges holding data of type b
data Graph a b = Graph (S.Set (Vertex a)) (M.Map Int [Edge a b]) deriving (Show)

-- For debugging
g = fromVertices [1#(Payload 2)]

-- |Create a graph from a list of vertices, with no edges
fromVertices :: [Vertex a] -> Graph a b
fromVertices vs = Graph (S.fromList vs) M.empty

-- |Create an edge from a tuple of its components
edgeFromTuple :: (Vertex a, Vertex a, Payload b) -> Edge a b
edgeFromTuple (x, y, p) = Edge x y p

-- |Create a vertex from its Id and Payload
makeVertex :: Int -> Payload a -> Vertex a
makeVertex x p = Vertex x p

(#) = makeVertex

-- |Returns the Id of a vertex
vertexId :: Vertex a -> Int
vertexId (Vertex x _) = x

-- |Insert a vertex
insertVertex :: Graph a b -> Vertex a -> Graph a b
insertVertex (Graph vs p) v = Graph (S.insert v vs) p

-- |Insert an edge. If vertices are 
insertEdge :: Graph a b -> Edge a b -> Graph a b
insertEdge = undefined



