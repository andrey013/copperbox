{-# OPTIONS -Wall #-}


module Demo01 where

import ZSyn.Active
import ZSyn.Base
import qualified ZSyn.HSStream  as S
import ZSyn.HSStream  ( (*>), HSStream(..) )
import ZSyn.Seconds
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


oldmain :: IO ()
oldmain = outputWav_1Chan_16Bit "demo02.wav" sound2 sample_rate 4


main :: IO ()
main = outputWav_1Chan_16Bit "demo02.wav" (runActive abc) sample_rate 4
  where
    abc = simpleNote 440 1.0 `oplus` simpleNote 550 2.0



a440 :: AudioStream
a440 = oscil 440


env1 :: ControlStream 
env1 = lineSeg [0, 1.0, 0.0] [2,4]


sound1 :: AudioStream 
sound1 = (*) a440 (upSample env1)


env2 :: ControlStream
env2 = envelope 0.05 0.1 0.5 1.0 0.5 0.4 1.0

sound2 :: AudioStream 
sound2 = (*) a440 (upSample env2)
