{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Syn.NewSynth
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Head strict stream
--
--------------------------------------------------------------------------------


module Syn.NewSynth
  ( 
  -- 
    (*>)
  , zeros
  , ones
  , ints
  , wave
  , oscil
  , delay

--  , dcremove
  , allpass

  , karstr

  ) where 


import Syn.Stream

import Prelude hiding ( repeat, map, zip, take )
import Data.Bits
-- import qualified Data.List as L


(<|) = (:<)

--------------------------------------------------------------------------------

type SampleRate = Double

sample_rate :: SampleRate
sample_rate = 44100.0

dpiSR :: Double
dpiSR = 2*pi / sample_rate



zeros :: Stream Double
zeros = repeat 0

ones :: Stream Double
ones = repeat 1

ints :: Stream Double 
ints = 0 <| ints + ones

wave :: Stream Double
wave = map (\n -> sin (2 * pi * n * h)) ints  
  where 
    h = 440 / sample_rate
 
oscil :: Double -> Stream Double
oscil fr = y 
  where
    ohm = dpiSR * fr
    y   = 0 <| v
    v   = (sin ohm) <| ((2 * cos ohm) *> v - y)

{-
additive :: Double -> [Double] -> Stream Double
additive freq amplist = y
  where
    m = [1.0 .. fromIntegral $ length amplist]
    y = zip (+) zeros 
                    (L.map (\(n,a) -> a *> oscil (n * freq)) (L.zip m amplist))
-}

delay1 :: Num a => Stream a -> Stream a
delay1 s = 0 <| s

delay :: Num a => Int -> Stream a  -> Stream a
delay i s = go i
  where
    go n | n < 1 = s 
    go n         = 0 <| go (n -1)

{-
dcremove :: Double -> Stream Double -> Stream Double
dcremove a xs@(viewl -> (_ :< xq)) = y
  where
    y  = a *> delay1 y + ((1.0+a) / 2.0) *> (xq - xs)
-}

allpass :: Int -> Double -> Stream Double -> Stream Double
allpass m b x = b *> z + v
  where
    v = delay m z
    z = x - b *> v

rand1 :: Int -> (Double,Int)
rand1 seed = 
   let seed1  = 599479 + seed * 25781083
       r      = seed .&. 2147483647
   in (fromIntegral r / 2147483648.0, seed1)
  

rndnoise :: Int -> Stream Double 
rndnoise seed = 
   let (z,seed1) = rand1 seed
   in z <| rndnoise seed1

karstr :: Double -> Stream Double
karstr f = y
  where
    prfx = take (ceiling (sample_rate/f)) (rndnoise 5478423)
    y    = prfx << 0.5 *> (y + delay1 y)


