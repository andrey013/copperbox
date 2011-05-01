{-# OPTIONS -Wall #-}


module Demo01 where

import ZSyn

import Data.VectorSpace                         -- package: vector-space

import Data.Word
import System.Directory


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
    v   = (sin ohm) :< ((2 * cos ohm) *^ v - y)


oldmain :: IO ()
oldmain = outputWav_1Chan_16Bit "out02.wav" sound2 sample_rate 4


main :: IO ()
main = do
    createDirectoryIfMissing True "./out/"
    outputWav_1Chan_16Bit "./out/demo06.wav" (runActive efg) sample_rate 4
  where
    abc = amplify 0.5 $ at 0.5 $ (simpleNote 440 1.0 `oplus` simpleNote 550 2.0)
                `over` simpleNote 330 2.0
  
    bcd = sawNote 440 1.0
    cde = triNote 440 1.0
    def = squNote 440 1.0
    efg = decorate (allpass 10 0.2) def



amplify :: Double -> Active p Double -> Active p Double
amplify d = fmap (*d)


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
