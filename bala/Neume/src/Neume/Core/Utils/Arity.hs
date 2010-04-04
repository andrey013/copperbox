{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.Arity
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Arity families of functions 
--
--------------------------------------------------------------------------------

module Neume.Core.Utils.Arity
  ( 

  -- * Specs!  
    oo
  , ooo 
  , oooo

  , ptranspose2
  , ptranspose3


  ) where

--------------------------------------------------------------------------------
-- 'specs'


-- | Compose an arity 1 function with an arity 2 function.
-- B1 - blackbird
oo :: (a -> ans) -> (r1 -> r2 -> a) -> r1 -> r2 -> ans
oo f g x y = f (g x y)

-- | Compose an arity 1 function with an arity 3 function.
-- B2 - bunting
ooo :: (a -> ans) -> (r1 -> r2 -> r3 -> a) -> r1 -> r2 -> r3 -> ans
ooo f g x y z = f (g x y z)

-- | Compose an arity 1 function with an arity 4 function.
oooo :: (a -> ans) -> (r1 -> r2 -> r3 -> r4 -> a) -> r1 -> r2 -> r3 -> r4 -> ans
oooo f g x y z1 z2 = f (g x y z1 z2)

-- transposing tuples

ptranspose2 :: (a1,b1) -> (a2,b2) -> ((a1,a2),(b1,b2))
ptranspose2 (a1,b1) (a2,b2) = ((a1,a2),(b1,b2))

ptranspose3 :: (a1,b1) -> (a2,b2) -> (a3,b3) -> ((a1,a2,a3),(b1,b2,b3))
ptranspose3 (a1,b1) (a2,b2) (a3,b3) = ((a1,a2,a3),(b1,b2,b3))
