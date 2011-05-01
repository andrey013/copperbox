{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSyn.Filters
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Filters.
--
--------------------------------------------------------------------------------


module ZSyn.Filters
  (

    lowpass
  , hipass

  , allpass

  ) where


import ZSyn.HSStream

import Data.VectorSpace                         -- package: vector-space

lowpass :: HSStream p Double -> HSStream p Double
lowpass x = y
  where
    y        = 0.5 *^ (x ^+^ delay1 x)
    delay1 s = 0 :< s



hipass :: HSStream p Double -> HSStream p Double
hipass x = y
  where
    y        = 0.5 *^ (x ^-^ delay1 x)
    delay1 s = 0 :< s

allpass :: Int -> Double -> HSStream p Double -> HSStream p Double
allpass m b x = y
  where
    y        = b *^ z ^+^ v
    v        = delay m z
    z        = x ^-^ b *^ v

    delay i s | i < 1 = s
              | otherwise = 0 :< delay (i-1) s