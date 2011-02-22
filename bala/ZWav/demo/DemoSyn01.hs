{-# OPTIONS -Wall #-}


module Main where

import Syn.Output16
import Syn.Synth

import ZWav.Datatypes hiding ( sample_rate )
import ZWav.WriteFile
import ZWav.ReadFile ( readWav )

import Data.HeadStrictStream                    -- package: head-strict-stream
import qualified Data.HeadStrictStream  as HSS



main :: IO ()
main = demo01


demo01 :: IO ()
demo01 = writeWav "demo01.wav" $  makeWav_OneChan 40000 (fastSine 440 sample_rate) 


{-


demo02 :: IO ()
demo02 = writeWav "demo02.wav" $  makeWav_OneChan 400000 wave

demo03 :: IO ()
demo03 = writeWav "demo03.wav" $  makeWav_OneChan 400000 s 
  where
   s = dcremove 0.75 $ oscil 440

demo04 :: IO ()
demo04 = writeWav "demo04.wav" $  makeWav_OneChan 400000 s 
  where
   s = allpass 30000 0.75 $ oscil 440


demo05 :: IO ()
demo05 = writeWav "demo05.wav" $  makeWav_OneChan 400000 s 
  where
   s = karstr 440
-}