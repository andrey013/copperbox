{-# OPTIONS -Wall #-}


module Demo01 where

import ZSyn.Base
import qualified ZSyn.HSStream  as S
import ZSyn.HSStream  ( (*>), HSStream(..) )
import ZSyn.WavHeader
import ZSyn.WavOutput

import Data.Word


type SampleRate = Word32

sample_rate :: SampleRate
sample_rate = 44100

dpiSR :: Double
dpiSR = 2*pi / (fromIntegral sample_rate)

oscil :: Double -> AudioStream
oscil fr = y 
  where
    ohm = dpiSR * fr
    y   = 0 :< v
    v   = (sin ohm) :< ((2 * cos ohm) *> v - y)


main :: IO ()
main = outputWav_1Chan_16Bit "demo01.wav" sound1 sample_rate 400000

a440 :: AudioStream
a440 = oscil 440


env :: ControlStream 
env = lineSeg [0, 1.0, 0.0] [2,4]


sound1 :: AudioStream 
sound1 = (*) a440 (upSample env)
