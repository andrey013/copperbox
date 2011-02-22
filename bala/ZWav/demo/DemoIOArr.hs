{-# OPTIONS -Wall #-}


module Main where

import Syn.NewSynth
import Syn.OutputIOArray
import qualified Syn.Stream as S

import Data.Bits
import Data.Word


main = outputWAV 400000 "newarr.wav" a440

a440 = oscil 440


test01 = S.take 50 a440

demo01 = temp_runBuilder 100 $ do 
    riffHeader 1000 

big32 :: Word32 -> IO ()
big32 w = do  
    print (w8 $ shiftR w 24) 
    print (w8 $ shiftR w 16) 
    print (w8 $ shiftR w 8) 
    print (w8 w) 


w8 :: Integral a => a -> Word8
w8 = fromIntegral