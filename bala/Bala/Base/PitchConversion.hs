
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.PitchConversion
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Convert pitch representations to other representations
-- |
--------------------------------------------------------------------------------

module Bala.Base.PitchConversion where

import Bala.Base.PitchRep



  
newtype MidiPitch = M {unMidi :: Int}

instance EncodePitch MidiPitch where
  fromPitch (Pitch l a o _) = mkMidi (root l) a o

  toPitch (M a) = Pitch pch shp oct 0
    where 
      (pch,shp) = noteOf a
      oct       = (a `div` 12) - 1 
      noteOf m = sharpNote (m `mod` 12)

      sharpNote 0   = (C,Nat)
      sharpNote 1   = (C,Sharp)
      sharpNote 2   = (D,Nat)
      sharpNote 3   = (D,Sharp)
      sharpNote 4   = (E,Nat)
      sharpNote 5   = (F,Nat)
      sharpNote 6   = (F,Sharp)
      sharpNote 7   = (G,Nat)
      sharpNote 8   = (G,Sharp)
      sharpNote 9   = (A,Nat)
      sharpNote 10  = (A,Sharp)
      sharpNote 11  = (B,Nat)
      sharpNote _   = error "noteOf"
      
    


mkMidi offst accdt octv = M $ offst + alteration accdt + octaveMidi octv
  where octaveMidi oct            = (1 + oct) * 12









hzPC :: Float -> Float
hzPC hz = y + (12.0*z) / 100.0
  where
    k :: Float
    k = 8.75 + logBase 2.0 (hz / 440.0)
    
    y = fromIntegral $ round k
    
    z = k - y

hzMidi :: Float -> Int
hzMidi hz = round $ (12*((log hz / log 2.0)-a)) - 3
  where
    a = log 6.875 / log 2.0
      

hzOct :: Float -> Float
hzOct hz = log (hz / 1.021975) / 0.69314718

midiHz :: Int -> Float 
midiHz m = 6.875 * (2.0 ** ((fromIntegral $ m+3) / 12)) 

midiOct :: Int -> Float 
midiOct m = 8.0 + (fromIntegral m -60)/12.0
  

midiPC :: Int -> Float
midiPC m = ((frac*12.0) / 100.0) + fromIntegral i
  where
    i :: Int
    i = 8 +  floor ((fromIntegral m - 60.0) / 12.0)
    frac :: Float
    frac = (fromIntegral $ m - (60 +(12*(i-8)))) / 12.0
    
octHz :: Float -> Float
octHz oct = 1.021975 * (2.0 ** x)
  where
    x = fromIntegral $ floor oct

octMidi :: Float -> Int
octMidi oct = floor $ k + 0.5
  where
    k = (12.0*(oct-8.0)) + 60.0
  

octPC :: Float -> Float
octPC oct = (0.12*(oct-x)) + x
  where
    x = fromIntegral $ floor oct 


        
pcMidi :: Float -> Int
pcMidi pc = 60 + (12*(i-8)) + fromEnum ((100.0*(pc - (fromIntegral i)))+0.5)
  where i ::Int
        i = floor pc
    


pcHz :: Float -> Float
pcHz pc = (2.0 ** (oct + (8.333333 * (pc - oct)))) * 1.021975
  where
    oct :: Float
    oct = fromIntegral $ floor pc
  
        
pcOct :: Float -> Float        
pcOct pc = x + (8.33333 * (pc - x))
  where
    x = fromIntegral $ floor pc
    
    

    
    