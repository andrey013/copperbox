{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Arity.Functions.PStar
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Arity extended combinators
-- 
-- Permutated starlings
--
--------------------------------------------------------------------------------

module Data.Arity.Functions.PStar
  (
    pstar
  , pstar2
  , pstar3
  , pstar4
  , pstar5

  ) where



-- Permutated starlings ...

pstar     :: (a -> r -> ans) 
          -> (r -> a) 
          -> r -> ans
pstar f fa x = f (fa x) x


pstar2    :: (a -> b -> r -> ans) 
          -> (r -> a) -> (r -> b) 
          -> r -> ans
pstar2 f fa fb x = f (fa x) (fb x) x

pstar3    :: (a -> b -> c -> r -> ans) 
          -> (r -> a) -> (r -> b) -> (r -> c) 
          -> r -> ans
pstar3 f fa fb fc x = f (fa x) (fb x) (fc x) x

pstar4    :: (a -> b -> c -> d -> r -> ans) 
          -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) 
          -> r -> ans
pstar4 f fa fb fc fd x = f (fa x) (fb x) (fc x) (fd x) x

pstar5    :: (a -> b -> c -> d -> e -> r -> ans) 
          -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) -> (r -> e) 
          -> r -> ans
pstar5 f fa fb fc fd fe x = f (fa x) (fb x) (fc x) (fd x) (fe x) x
