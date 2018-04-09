module Memory where


data TypeName   = GInt | GFloat | GChar | GString | GBool | GList deriving (Show)
data Value      = Integer Integer | Float Double | Char Char | String String | Bool Bool | List [Value] deriving (Show)
data Cell       = Cell (String, String, Value) deriving (Show)

getType :: Value -> TypeName
getType x = case x of
                Integer _ -> GInt
