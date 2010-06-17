{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Syn.StreamDouble
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Head strict stream
--
--------------------------------------------------------------------------------


module Syn.StreamDouble
  ( 
  -- 
    ones
  , ints
  , wave
  , oscil

  ) where 

import Syn.Stream

import Prelude hiding ( repeat, map )

type SampleRate = Double

sample_rate :: SampleRate
sample_rate = 44100.0

dpiSR :: Double
dpiSR = 2*pi / sample_rate


ones :: Stream Double
ones = repeat 1

ints :: Stream Double 
ints = 0 :< ints + ones

wave :: Stream Double
wave = map (\n -> sin (2 * pi * n * h)) ints  
  where h = 440/sample_rate
 
oscil :: Double -> Stream Double
oscil fr = y where
  ohm = dpiSR * fr
  y   = 0 :< v
  v   = (sin ohm) :< ((2 * cos ohm) *> v - y)