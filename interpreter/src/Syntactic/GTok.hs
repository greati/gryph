{-# LANGUAGE FlexibleContexts #-}

module GTok where


import Lexer

import Text.Parsec hiding (satisfy, string)

type Parser = Parsec [Token] ()

satisfy :: (Stream [Token] m Token) => (Token -> Bool) -> ParsecT [Token] u m Token
satisfy f = tokenPrim show nextPos tokeq
    where 
        tokeq :: Token -> Maybe Token
        tokeq t = if f t then Just (fst t) else Nothing

nextPos = undefined
