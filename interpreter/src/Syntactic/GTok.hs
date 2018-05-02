{-# LANGUAGE FlexibleContexts #-}

module GTok where


import Lexer
import GphTokens

import Text.Parsec hiding (satisfy, string)

type Parser = Parsec [GphTokenPos] ()

satisfy :: (Stream [GphTokenPos] m GphTokenPos) => (GphTokenPos -> Bool) -> ParsecT [GphTokenPos] u m GphToken
satisfy f = tokenPrim show nextPos tokeq
    where 
        tokeq :: GphTokenPos -> Maybe GphToken
        tokeq t = if f t then Just (fst t) else Nothing

nextPos :: SourcePos -> GphTokenPos -> [GphTokenPos] -> SourcePos
nextPos pos _ ((_, AlexPn _ l c):_) = setSourceColumn (setSourceLine pos l) c
nextPos pos _ [] = pos

satisfy' :: (Stream [GphTokenPos] m GphTokenPos) => (GphTokenPos -> Maybe a) -> ParsecT [GphTokenPos] u m a
satisfy' = tokenPrim show nextPos

-- | Parses given `GphTokenPosen`.
tok :: (Stream [GphTokenPos] m GphTokenPos) => GphToken -> ParsecT [GphTokenPos] u m GphToken
tok t = satisfy (\(t', _) -> t' == t) <?> show t

tok' :: (Stream [GphTokenPos] m GphTokenPos) => GphToken -> ParsecT [GphTokenPos] u m ()
tok' p = tok p >> return ()

anyIdent :: Monad m => ParsecT [GphTokenPos] u m String
anyIdent = satisfy' p <?> "ident"
    where 
        p (t,_) = case t of 
                    GTokIdentifier i -> Just i
                    _ -> Nothing    

anyType :: Monad m => ParsecT [GphTokenPos] u m String
anyType = satisfy' p <?> "type"
    where 
        p (t,_) = case t of 
                    GTokType i -> Just i
                    _ -> Nothing    

numberLit :: Monad m => ParsecT [GphTokenPos] u m String
numberLit = satisfy' p <?> "number"
    where 
        p (t,_) = case t of
                    GTokIntLit i -> Just i
                    GTokFloatLit i -> Just i
                    _ -> Nothing

