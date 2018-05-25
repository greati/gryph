module Execution.GraphSemantic where

import Syntactic.Values
import Syntactic.Syntax
import Execution.Graph as G
import Data.Set        as S
import Data.List       as L

-- |Create a bfs
bfs :: (Show a, Show b) => G.Graph a b -> Vertex a -> IO ()
bfs g v = do
    bfs' g v [] S.empty
    return ()

bfs' :: (Show a, Show b) => G.Graph a b -> Vertex a -> [Vertex a] -> S.Set (Vertex a) -> IO ()
bfs' g v queue vis = do
    if S.member v vis
    then do
        if queue == []
        then do
            return ()
        else do
            if L.length queue == 1
            then do
                bfs' g (L.head queue) [] vis
                return ()
            else do
                bfs' g (L.head queue) (L.tail queue) vis
                return ()
    else do
        putStrLn (show v)
        let n_vis    = S.insert v vis
        let n_queue  = queue ++ (verticesOfEdges (G.getEdges g v))

        if n_queue == []
        then do
            return ()
        else do
            if L.length n_queue == 1
            then do
                bfs' g (L.head queue) [] vis
                return () 
            else do
                bfs' g (L.head n_queue) (L.tail n_queue) n_vis 
                return ()

dfs :: (Show a, Show b) => G.Graph a b -> G.Vertex a -> IO ()
dfs g v = do
    dfs' g v [] S.empty
    return ()


dfs' :: (Show a, Show b) => G.Graph a b -> Vertex a -> [Vertex a] -> S.Set (Vertex a) -> IO ()
dfs' g v stack vis = do
    if S.member v vis
    then do
        if stack == []
        then do
            return ()
        else do
            if L.length stack == 1
            then do
                bfs' g (L.head stack) [] vis
                return ()
            else do
                bfs' g (L.head stack) (L.tail stack) vis
                return ()
    else do
        putStrLn (show v)
        let n_vis    = S.insert v vis
        let n_stack  = (verticesOfEdges (G.getEdges g v)) ++ stack

        if n_stack == []
        then do
            return ()
        else do
            if L.length n_stack == 1
            then do
                bfs' g (L.head stack) [] vis
                return () 
            else do
                bfs' g (L.head n_stack) (L.tail n_stack) n_vis 
                return ()

verticesOfEdges :: [G.Edge a b] -> [Vertex a]
verticesOfEdges [] = []
verticesOfEdges [G.Edge v1 v2 _] =  [v2]
verticesOfEdges ((G.Edge v1 v2 _):xs) = v2 : verticesOfEdges xs

