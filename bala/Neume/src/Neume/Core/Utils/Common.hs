{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.Common
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

module Neume.Core.Utils.Common 
  ( 
    makeRational
  , ftrunc
  , dtrunc
  , divModR
  , modR
  , strip

  , para
  , paraM

  -- * Specs!  
  , oo
  , ooo 
  , oooo


  ) where

import Data.Ratio


 
makeRational :: Integral a => a -> a -> Rational
makeRational a b = fromIntegral a % fromIntegral b



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
-- divMod (with rounding) for rationals 

-- check - 8.0 `divModR` 0.75

-- prop_mod_postive a b = let (_,md) = a `divModR` b in signum md == 1

divModR :: Integral a => Ratio a -> Ratio a -> (a, Ratio a)
divModR a b = let a1 = a / b; a2 = floor a1 in (a2, a-((a2%1)*b))

modR :: Integral a => Ratio a -> Ratio a -> Ratio a
modR = snd `oo` divModR

-- | strip - dual of const
--
strip :: a -> b -> b
strip _ b = b


para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para phi b = step
  where step []     = b
        step (x:xs) = phi x (xs, step xs)


paraM :: Monad m => (a -> ([a], b) -> m b) -> b -> [a] -> m b
paraM phi b = step
  where step []     = return b
        step (x:xs) = step xs >>= \ans -> phi x (xs, ans)

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


