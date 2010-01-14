{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ParsecExtras.HaskellLexer
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
-- 
--
--------------------------------------------------------------------------------


module Text.ParserCombinators.ParsecExtras.HaskellLexer
  (
    reserved

  , commentStart
  , lineComment

  ) where

import Text.ParserCombinators.ParsecExtras.HaskellLexer.Tokens

import Text.ParserCombinators.Parsec

import Control.Applicative ( liftA2 )

type HParser st a = GenParser Token st a


reserved :: String -> HParser st ()
reserved name = hsToken $ matchToken pRes (const ())
  where
    pRes = liftA2 (&&) (eqLexeme Reserved) (eqTokenString name)


commentStart :: HParser st ()
commentStart = hsToken $ matchToken (eqLexeme CommentStart) (const ())

lineComment :: HParser st String 
lineComment = hsToken $ matchToken (eqLexeme Comment) tokenSource

{-
comment :: HParser st String
comment = commentStart >> lineComment
-}


--------------------------------------------------------------------------------
-- 

hsToken :: (Token -> Maybe a) -> HParser st a
hsToken = tokenPrim showToken posToken 


matchToken :: (Token -> Bool) -> (Token -> a) -> Token -> Maybe a
matchToken p f a | p a       = Just $ f a
                 | otherwise = Nothing

eqLexeme :: Lexeme -> Token -> Bool
eqLexeme a (_,x,_) = a==x

eqTokenString :: String -> Token -> Bool
eqTokenString a (_,_,s) = a==s

showToken :: Token -> String
showToken (_,_,str) = str 

posToken  :: SourcePos -> Token -> [Token] -> SourcePos
posToken pos _ _ = pos


--------------------------------------------------------------------------------

{-

whenMb :: (a -> Bool) -> a -> Maybe a
whenMb p a | p a       = Just a
           | otherwise = Nothing


discard :: Monad m => m a -> m ()
discard ma = do { _ <- ma; return () } 

-}