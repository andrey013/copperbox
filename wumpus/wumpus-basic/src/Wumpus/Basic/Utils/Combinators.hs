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
-- Combiantors - pairing, /static argument/ functions, ...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Utils.Combinators
  (

  -- * Combinators
    fork
  , prod
  , forkA

  , bindR
  , bindAsk
  , bindInto
  , rlift1

  , bindR2
  , rlift2


  ) where

import Control.Applicative

fork :: (a -> b) -> (a -> c) -> a -> (b,c) 
fork f g a = (f a, g a)

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d) 
prod f g (a,b) = (f a, g b)


forkA :: Applicative f => f a -> f b -> f (a,b)
forkA af ab = (,) <$> af <*> ab 



infixl 1 `bindR`, `bindR2`

-- Monadic bind with 1 static argument.
--
bindR :: Monad m => (r -> m a) -> (a -> r -> m b) -> r -> m b
bindR cxma cxmf = \x -> cxma x >>= \a -> cxmf a x

-- 'bindAsk' takes a monadic function oblivious to R1 upto bindR.
-- (lift and bind).
--
bindAsk :: Monad m => m a -> (a -> r1 -> m b) -> r1 -> m b 
bindAsk mq cxmf r1 = mq >>= \a -> cxmf a r1


-- 'bindInto' takes a monadic action dependent on R1 and binds
-- it into a mondic function oblivious to R1. 
--
bindInto :: Monad m => (r1 -> m a) -> (a -> m b) -> r1 -> m b 
bindInto cxma mf r1 = cxma r1 >>= \a -> mf a

rlift1 :: Monad m => m a -> (r -> m a)
rlift1 ma = \_ -> ma



-- Monadic bind with 2 static arguments.
--
bindR2 :: Monad m 
       => (r1 -> r2 -> m a) -> (a -> r1 -> r2 -> m b) -> r1 -> r2 -> m b
bindR2 cxma cxmf = \x y -> cxma x y >>= \a -> cxmf a x y



rlift2 :: Monad m => m a -> (r1 -> r2 -> m a)
rlift2 ma = \_ _ -> ma

