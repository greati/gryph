module Execution.RepetitionSemantic where

import Syntactic.Syntax
import Syntactic.Values
import Execution.Memory
import Execution.Semantic
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

while :: Bool -> IO ()
while exp = do
    if exp == False
    then do
        return ()
    else do
        -- Execute something
        -- Provisional
        n_exp <- getLine
        if n_exp == "True"
        then do 
            while True
        else do
            while False
        return ()
        -- Correct
        -- while exp
        -- return ()
            

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

list_comp  = (ListComp (ArithBinExpr PlusBinOp (ArithTerm (IdTerm (Ident "a"))) (ArithTerm (IdTerm (Ident "b")))) (ForIterator [Ident "a",Ident "b"] [ExprLiteral (ListLit [ArithTerm (LitTerm (Lit (Integer 1))),ArithTerm (LitTerm (Lit (Integer 2))),ArithTerm (LitTerm (Lit (Integer 3)))]),ExprLiteral (ListLit [ArithTerm (LitTerm (Lit (Integer 5))),ArithTerm (LitTerm (Lit (Integer 6))),ArithTerm (LitTerm (Lit (Integer 7)))])] [ArithRelExpr Less (ArithBinExpr TimesBinOp (ArithTerm (IdTerm (Ident "a"))) (ArithTerm (LitTerm (Lit (Integer 5))))) (ArithTerm (LitTerm (Lit (Integer 10))))]))
list_comp' = (ListComp (ArithBinExpr PlusBinOp (ArithTerm (IdTerm (Ident "a"))) (ArithTerm (IdTerm (Ident "b")))) (ForIterator [Ident "a"] [ExprLiteral (ListLit [ArithTerm (LitTerm (Lit (Integer 1))),ArithTerm (LitTerm (Lit (Integer 2))),ArithTerm (LitTerm (Lit (Integer 3)))])] [ArithRelExpr Less (ArithBinExpr TimesBinOp (ArithTerm (IdTerm (Ident "a"))) (ArithTerm (LitTerm (Lit (Integer 5))))) (ArithTerm (LitTerm (Lit (Integer 10))))]))

overListComp :: [Value] -> [Value]
overListComp [EmptyList]         = [EmptyList] 
overListComp [(List xs)]         = [(List [x]) | x <- xs]
overListComp (List [x]: xss)   = joinListComp x (overListComp xss)
overListComp (List (x:xs):xss) = (joinListComp x (overListComp xss)) ++ (overListComp ((List xs): xss))


joinListComp :: Value -> [Value] -> [Value]
joinListComp v [EmptyList]     = [EmptyList]
joinListComp v [(List xs)]     = [ List (v : xs) ]
joinListComp v ((List xs):xss) = (List (v : xs)) : (joinListComp v xss)


evalListComp :: Memory -> ProgramMemory -> Scopes -> ListComp -> (ArithExpr, [Identifier], [Value], ArithExpr)
evalListComp m pm ss (ListComp expression (ForIterator is xs [when_exp] ) ) = (expression, is, overListComp (getLists [xs]), when_exp)
    where
        getLists (xs:[])  = (evalList m pm ss xs)
        getLists (xs:xss) = (evalList m pm ss xs) ++ getLists xss


