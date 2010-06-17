{-# OPTIONS -Wall #-}


module DemoSyn01 where

import Syn.Output16
import Syn.Stream
import Syn.StreamDouble

import ZWav.WriteFile
import ZWav.ReadFile ( readWav )
import Control.Exception
import Prelude hiding (catch, map )

import System.Exit


test01 = toInt 1 == 0x7FFF
test02 = toInt (-1) == -(0x8000)


demo01 = writeWav "demo02.wav" $  makeWav 400000 wave -- (oscil 220) 

demo02 = readWav "demo01.wav"

{-

test01 :: IO ()
test01 = process "wavfiles/min.wav"

test02 :: IO ()
test02 = process "wavfiles/sine_44k_s.wav"

process :: FilePath -> IO ()
process filename = do
    ans <- catch (readWav filename) exitHandle
    print ans
  where
    exitHandle :: IOException -> IO a 
    exitHandle e = putStrLn (show e) >> exitFailure

-}