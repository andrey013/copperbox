{-# OPTIONS -Wall #-}


module Demo01 where

import ZSyn.HSStream
import ZSyn.WavHeader
import ZSyn.WavOutput

import Data.Word


type SampleRate = Word32

sample_rate :: SampleRate
sample_rate = 44100

dpiSR :: Double
dpiSR = 2*pi / (fromIntegral sample_rate)

oscil :: Double -> HSStream Double
oscil fr = y 
  where
    ohm = dpiSR * fr
    y   = 0 :< v
    v   = (sin ohm) :< ((2 * cos ohm) *> v - y)


main = outputWav_1Chan_16Bit "newarr.wav" a440 sample_rate 400000

a440 = oscil 440