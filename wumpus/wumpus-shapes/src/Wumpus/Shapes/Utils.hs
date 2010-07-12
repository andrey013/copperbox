{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Combinators...
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Utils
  ( 

  -- * Starlings...
    star
  , star2
  , star3
  , star4 

  ) where

-- starlings...

-- Note - this is the correct argument order for a starling.
-- I\'ve used these combinators elsewhere with arguments of the
-- \"combiner\" in the wrong order.

star     :: (r -> a -> ans) 
         -> (r -> a) 
         -> r -> ans
star f fa x = f x (fa x)

star2    :: (r -> a -> b -> ans) 
         -> (r -> a) 
         -> (r -> b) 
         -> r -> ans
star2 f fa fb x = f x (fa x) (fb x)

star3    :: (r -> a -> b -> c -> ans) 
         -> (r -> a) 
         -> (r -> b) 
         -> (r -> c) 
         -> r -> ans
star3 f fa fb fc x = f x (fa x) (fb x) (fc x)

star4    :: (r -> a -> b -> c -> d -> ans) 
         -> (r -> a) 
         -> (r -> b) 
         -> (r -> c)
         -> (r -> d) 
         -> r -> ans
star4 f fa fb fc fd x = f x (fa x) (fb x) (fc x) (fd x)


{-

-- |  - aka liftM on functions 
-- 
esop :: (r -> a) -> (a -> ans) -> r -> ans
esop g f r = f (g r)

-- | bigphi2 - a
-- 
esop2 :: (r -> a) -> (r -> b) -> (a -> b -> ans) -> r -> ans
esop2 fa fb f r = f (fa r) (fb r)
-}

