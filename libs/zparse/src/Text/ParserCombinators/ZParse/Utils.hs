{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common utils
--
--------------------------------------------------------------------------------

module Text.ParserCombinators.ZParse.Utils 
  (
    subst
  , subst2
  , oo
  , ooo
  , oooo

  ) where


-- aka (<*>), for some uses I prefer subst in the prefix form...

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x)

subst2 :: (a -> b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
subst2 f g h x = f x (g x) (h x)

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