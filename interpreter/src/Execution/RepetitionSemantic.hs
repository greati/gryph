module Execution.Repetition where

import Data.List

over :: [[a]] -> [[a]]
over [[]]         = [[]] 
over [xs]         = [[x] | x <- xs]
over ([x]:xss)    = (join x (over xss))
over ((x:xs):xss) = (join x (over xss)) ++ (over ((xs):xss))

join i [[]]   = [[]]
join i [x]    = [(i : x)]
join i (x:xs) = (i : x) : (join i xs)

