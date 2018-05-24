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

for :: (Eq b, Show b, Show a, Ord a) => [a] -> [[b]] -> Bool -> IO ()
for is xss exp = do 
    let (ys:yss) = over xss
    -- Memory simulation
    let lt       = zip is ys
    let memory   = M.fromList lt
    for' is yss memory exp
    return ()

for' :: (Eq b, Show a, Show b, Ord a) => [a] -> [[b]] -> M.Map a b -> Bool -> IO ()
for' is xss memory exp = do 
    if xss /= [] 
    then do
        if exp == True
        then do
            putStrLn (show memory)
            
            -- Execute something
            
            let memory' = updateMemory is (L.head xss) memory
            (for' is (L.tail xss) memory' exp)
        
        else do
            let memory' = updateMemory is (L.head xss) memory
            (for' is (L.tail xss) memory' exp)
    
    else do
        if exp == True
        then do 
            putStrLn (show memory)
            return ()
        else do return () 

updateMemory :: Ord a => [a] -> [b] -> (M.Map a b) -> (M.Map a b)
updateMemory [x] [y] mem       = M.insert x y mem
updateMemory (x:xs) (y:ys) mem = updateMemory xs ys (M.insert x y mem)
     
