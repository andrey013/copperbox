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
  , both
  , prod


  , psimap
  , mapInto
  , anacrusisMap
  , cabooseMap

  , unlist1

  , para
  , paraM



  ) where


import Neume.Core.Utils.Arity

import Data.Ratio
import Data.Sequence

 
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


-- Pairs

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)


prod :: (a -> s) -> (b -> t) -> (a,b) -> (s,t)
prod f g (a,b) = (f a, g b)


----

-- Rather like on (aka psi) from Data.Function 
psimap :: (a -> b) -> a -> [a] -> (b,[b])
psimap f x xs = (f x, map f xs)


mapInto :: (a -> b) -> Seq a -> [b]
mapInto f = step [] . viewr where
  step acc (sa :> a) = step (f a : acc) (viewr sa)
  step acc _         = acc




anacrusisMap :: (a -> b) -> (a -> b) -> [a] -> [b]
anacrusisMap _ _ []     = []
anacrusisMap f g (a:as) = f a : map g as

cabooseMap :: (a -> b) -> (a -> b) -> [a] -> [b]
cabooseMap _ _ []     = []  
cabooseMap f g (a:as) = step a as where
  step x []           = [g x]
  step x (y:ys)       = f x : step y ys


unlist1 :: (a -> b) -> [a] -> Maybe (b,[a])
unlist1 _ []     = Nothing
unlist1 f (a:as) = Just (f a, as)


para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para phi b = step
  where step []     = b
        step (x:xs) = phi x (xs, step xs)


paraM :: Monad m => (a -> ([a], b) -> m b) -> b -> [a] -> m b
paraM phi b = step
  where step []     = return b
        step (x:xs) = step xs >>= \ans -> phi x (xs, ans)

