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
modR a b = snd $ divModR a b

-- | strip - dual of const
--
strip :: a -> b -> b
strip _ b = b

