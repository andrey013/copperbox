{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Type classes and common things.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.Utils
  (
  -- * Specs
    oo
  , ooo
  , oooo

  -- * Combinators
  , appro
  , combfi
  , dup

  -- * Hughes list
  , H 
  , toListH
  , consH

  ) where

-- | Compose an arity 1 function with an arity 2 function.
-- B1 - blackbird
oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

-- | Compose an arity 1 function with an arity 3 function.
-- B2 - bunting
ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

-- | Compose an arity 1 function with an arity 4 function.
oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g  

-- NEEDS NEW NAME!
-- > on = (appro comb f f)
--
appro :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
appro comb f g x y = comb (f x) (g y) 


-- 
combfi :: (c -> b -> d) -> (a -> c) -> a -> b -> d
combfi comb f x y = comb (f x) y 

-- | dup - duplicator aka the W combinator aka Warbler. 
-- 
-- > dup f x = f x x
--
dup :: (a -> a -> b) -> a -> b
dup f x = f x x


--------------------------------------------------------------------------------

type H a = [a] -> [a]

toListH :: H a -> [a]
toListH = ($ [])

consH :: a -> H a
consH a = (a:)