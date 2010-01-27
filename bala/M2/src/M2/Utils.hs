{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  M2.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common utils...
--
--------------------------------------------------------------------------------

module M2.Utils 
  ( 
    ftrunc
  , dtrunc

  -- * Specs!  
  , oo
  , ooo 
  , oooo


  ) where


--------------------------------------------------------------------------------

ftrunc :: Float -> String
ftrunc = dtrunc . realToFrac


dtrunc :: Double -> String
dtrunc d | abs d < 0.0001  = "0.0"
         | d < 0.0           = '-' :  show (abs tx)
         | otherwise         = show tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0
 
    roundi :: RealFrac a => a -> Integer
    roundi = round

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


