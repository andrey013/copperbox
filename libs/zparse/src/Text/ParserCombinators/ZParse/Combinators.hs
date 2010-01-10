{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.Combinators
-- Copyright   :  (c) Stephen Tetley 2008, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Applicative and monadic combinators that only need the 
-- standard classes (Applicative, Alernative, MonadPlus...).
--
--------------------------------------------------------------------------------


module Text.ParserCombinators.ZParse.Combinators  
  ( 
   (<:>)
 , choice
 , count
 , between
 , option
 , optionMaybe
 , optionUnit
 , skipMany
 , skipMany1
 , many1
 , sepBy 
 , sepBy1
 , sepEndBy
 , sepEndBy1
 , manyTill
 , manyTill1

 , chainl1
 , chainr1
 , chainl
 , chainr

 ) where

import Control.Applicative
import Control.Monad

-- | Applicative cons.
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

-- | Applicative T - reverse fmap.
(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip fmap


choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty 
   
count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 
          
between :: Applicative f => f open -> f close -> f a -> f a
between o c a = o *> a <* c

          
option :: Alternative f => a -> f a -> f a
option x p          = p <|> pure x

optionMaybe :: Alternative f => f a -> f (Maybe a)
optionMaybe = optional

-- aka Parsecs /optional/
optionUnit :: Alternative f => f a -> f ()
optionUnit p = () <$ p <|> pure ()


skipMany :: Alternative f => f a -> f ()
skipMany p = many_p
  where many_p = some_p <|> pure ()
        some_p = p       *> many_p

skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

-- | 'many1' an alias for Control.Applicative 'some'. 
many1 :: Alternative f => f a -> f [a]
many1 = some

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = p <:> step where
    step = (sep *> p) <:> step <|> pure []

sepEndBy :: Alternative f => f a -> f b -> f [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

sepEndBy1 :: Alternative f => f a -> f b -> f [a]
sepEndBy1 p sep = (p <* sep) <:> step where
    step = (p <* sep) <:> step <|> pure []
    
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = step <|> pure [] where
    step = p <:> ((end <#> pure[]) <|> step)

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p end = p <:> step where
    step = (end <#> pure []) <|> (p <:> step)
    

-- The chain parsers need @bind@ so they have to be monadic 
-- rather than applicative
--


chainl1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p >>= rest 
  where rest x = mplus (op >>= \f -> p >>= \a -> rest (f x a)) (return x) 
               
chainr1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan 
   where 
     scan   = p >>= rest 
     rest x = mplus (op >>= \f -> scan >>= \a -> rest (f x a)) (return x) 

chainl :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainl p op v = mplus (chainl1 p op) (return v)

chainr :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainr p op v = mplus (chainr1 p op) (return v)


