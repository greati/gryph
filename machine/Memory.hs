module Memory where

data TypeName   = GInt | GFloat | GChar | GString | GBool | GList deriving (Show)
data Value      = Integer Integer | Float Double | Char Char | String String | Bool Bool | List [Value] deriving (Show)
type Cell       = (String, String, Value)

type MemoryState = [Cell]

getType :: Value -> TypeName
getType x = case x of
                Integer _ -> GInt

insert :: Cell -> MemoryState -> MemoryState
insert k' [] = [k']
insert k'@(k,_,v) (x'@(x,y,z):xs)
    | k == x   = (k,y,v):xs
    | otherwise = x' : insert k' xs 
