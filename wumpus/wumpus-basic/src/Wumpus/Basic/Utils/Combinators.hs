{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Utils.Combinators
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pairing functions... 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Utils.Combinators
  (

  -- * Combinators
    fork
  , prod
  , forkA
  , bindR

  ) where

import Control.Applicative

fork :: (a -> b) -> (a -> c) -> a -> (b,c) 
fork f g a = (f a, g a)

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d) 
prod f g (a,b) = (f a, g b)


forkA :: Applicative f => f a -> f b -> f (a,b)
forkA af ab = (,) <$> af <*> ab 



infixl 1 `bindR`
-- Monadic bind with 1 static argument.
--
bindR :: Monad m => (r -> m a) -> (a -> r -> m b) -> r -> m b
bindR ma mf = \x -> ma x >>= \a -> mf a x
