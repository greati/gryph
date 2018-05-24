module Execution.RepetitionSemantic where

import Syntactic.Syntax          as Sy
import qualified Data.List       as L
import qualified Data.Map.Strict as M

over :: [[a]] -> [[a]]
over [[]]         = [[]] 
over [xs]         = [[x] | x <- xs]
over ([x]:xss)    = (join x (over xss))
over ((x:xs):xss) = (join x (over xss)) L.++ (over ((xs):xss))

join i [[]]   = [[]]
join i [x]    = [(i : x)]
join i (x:xs) = (i : x) : (join i xs)

--for :: [String] -> [[a]] -> IO ()
for is xss = do 
    let (ys:yss) = over xss
    -- Memory simulation
    let lt       = zip is ys
    let memory   = M.fromList lt
    return (for' is yss memory)

for' is xss memory = do 
    if xss /= [] 
    then do
        -- Execute something
        let memory' = updateMemory is (L.head xss) memory
        (for' is (L.tail xss) memory')
    else do
        (memory)
    

updateMemory :: Ord a => [a] -> [b] -> (M.Map a b) -> (M.Map a b)
updateMemory [x] [y] mem       = M.insert x y mem
updateMemory (x:xs) (y:ys) mem = updateMemory xs ys (M.insert x y mem)
     
