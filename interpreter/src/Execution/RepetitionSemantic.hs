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

overListComp :: [Value] -> IO [Value]
overListComp [EmptyList]       = do return [EmptyList] 
overListComp [(List xs)]       = do return [(List [x]) | x <- xs]
overListComp (List [x]: xss)   = do xss' <- (overListComp xss)
                                    xss'' <- (joinListComp x xss')
                                    return xss''
overListComp (List (x:xs):xss) = do xss'  <- (overListComp xss)
                                    xss'' <- (joinListComp x xss') 
                                    xss''' <- (overListComp ((List xs): xss))
                                    return (xss'' ++ xss''')

joinListComp :: Value -> [Value] -> IO [Value]
joinListComp v [EmptyList]     = do return [EmptyList]
joinListComp v [(List xs)]     = do return [ List (v : xs) ]
joinListComp v ((List xs):xss) = do xss' <- (joinListComp v xss)
                                    return ((List (v : xs)) : xss')

evalListComp :: Memory -> ProgramMemory -> Scopes -> ListComp -> IO (ArithExpr, [Identifier], [Value], ArithExpr)
evalListComp m pm ss (ListComp expression (ForIterator is xs [when_exp] ) ) = do
        xss <- (getLists m pm ss [xs])
        xss' <- (overListComp xss)
        return (expression, is, xss', when_exp)
    where getLists m pm ss (xs:[])  = do xss <- (evalList m pm ss xs)
                                         return xss
          getLists m pm ss (xs:xss) = do xss' <- (evalList m pm ss xs) 
                                         xss'' <- (getLists m pm ss xss)
                                         return (xss' ++ xss'')

forListComp :: Memory -> ProgramMemory -> Scopes -> ListComp -> IO [Value]
forListComp m pm ss lc = do (exp, id, vs'@(v:vs), when_exp) <- evalListComp m pm ss lc
                            return [EmptyList]

forListComp' :: Memory -> ProgramMemory -> Scope -> ArithExpr -> [Identifier] -> [Value] -> ArithExpr -> IO [Value]
forListComp' m pm ss exp id [v] when_exp = do
                                                (Right m') <- updateListIds m [ss] id v
                                                success <- (eval m' pm [ss] when_exp)
                                                if success == (Bool True)
                                                then do
                                                    e <- eval m' pm [ss] exp 
                                                    return [e]
                                                else do 
                                                    return [EmptyList]

forListComp' m pm ss exp id (v:vs) when_exp = undefined   

updateListIds :: Memory -> Scopes -> [Identifier] -> Value -> IO (Either String Memory)
updateListIds m ss [(Ident id)] (List [v])        = do 
                                                        let r = updateVar m id ss ((getType v), (Value v))
                                                        return r
updateListIds m ss ((Ident id):ids) (List (v:vs)) = do
                                                        let (Right m') = updateVar m id ss ((getType v), (Value v))
                                                        r <- updateListIds m' ss ids (List vs)
                                                        return r