{-# OPTIONS -Wall #-}


module DemoSyn01 where

import Syn.Output16
-- import qualified Syn.Stream as S
import Syn.StreamDouble

import ZWav.Datatypes
import ZWav.WriteFile
import ZWav.ReadFile ( readWav )


test01, test02 :: Bool
test01 = toInt 1 == 0x7FFF
test02 = toInt (-1) == -(0x8000)

demo01 :: IO ()
demo01 = writeWav "demo03.wav" $  makeWav 400000 1 (oscil 440) 

demo02 :: IO WavFile
demo02 = readWav "demo02.wav"


