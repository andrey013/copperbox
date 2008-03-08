

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
import Bala.Base.BaseExtra


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

  
newtype MidiPitch = M {unMidi :: Int}
  deriving (Eq)

newtype Hertz = Hz {unHertz :: Float}
  deriving (Eq)
  
-- pitch class with an octave designation
newtype OctavePC = OPC {unOPC :: Float}
  deriving (Eq)
  
newtype OctaveRep = OR {unOR :: Float}
  deriving (Eq)
  
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

hertzFrom :: Float -> Hertz
hertzFrom = Hz

midiForm :: Int -> MidiPitch
midiForm = M 

picthClassForm :: Float -> OctavePC
picthClassForm = OPC

octaveForm :: Float -> OctaveRep
octaveForm = OR


instance EncodePitch MidiPitch where
  fromPitch (Pitch l a o _) = mkMidi (semis l) a o

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
      
    


mkMidi offst accdt octv = M $ offst + semis accdt + octaveMidi octv
  where octaveMidi oct            = (1 + oct) * 12



hzPC :: Hertz -> OctavePC
hzPC (Hz hz) = OPC $ y + (12.0*z) / 100.0
  where
    k :: Float
    k = 8.75 + logBase 2.0 (hz / 440.0)
    
    y = fromIntegral $ round k
    
    z = k - y

hzMidi :: Hertz -> MidiPitch
hzMidi (Hz hz) = M $ round $ (12*((log hz / log 2.0)-a)) - 3
  where
    a = log 6.875 / log 2.0
      

hzOct :: Hertz -> OctaveRep
hzOct (Hz hz) = OR $ log (hz / 1.021975) / 0.69314718

midiHz :: MidiPitch -> Hertz 
midiHz (M m) = Hz $ 6.875 * (2.0 ** ((fromIntegral $ m+3) / 12)) 

midiOct :: MidiPitch -> OctaveRep 
midiOct (M m) = OR $ 8.0 + (fromIntegral m -60)/12.0
  

midiPC :: MidiPitch -> OctavePC
midiPC (M m) = OPC $ ((frac*12.0) / 100.0) + fromIntegral i
  where
    i :: Int
    i = 8 +  floor ((fromIntegral m - 60.0) / 12.0)
    frac :: Float
    frac = (fromIntegral $ m - (60 +(12*(i-8)))) / 12.0
    
octHz :: OctaveRep -> Hertz
octHz (OR oct) = Hz $ 1.021975 * (2.0 ** x)
  where
    x = fromIntegral $ floor oct

octMidi :: OctaveRep -> MidiPitch
octMidi (OR oct) = M $ floor $ k + 0.5
  where
    k = (12.0*(oct-8.0)) + 60.0
  

octPC :: OctaveRep -> OctavePC
octPC (OR oct) = OPC $ (0.12*(oct-x)) + x
  where
    x = fromIntegral $ floor oct 


        
pcMidi :: OctavePC -> MidiPitch
pcMidi (OPC pc) = M $ 60 + (12*(i-8)) + fromEnum ((100.0*(pc - (fromIntegral i)))+0.5)
  where i ::Int
        i = floor pc
    


pcHz :: OctavePC -> Hertz
pcHz (OPC pc)  = Hz $ (2.0 ** (oct + (8.333333 * (pc - oct)))) * 1.021975
  where
    oct :: Float
    oct = fromIntegral $ floor pc
  
        
pcOct :: OctavePC -> OctaveRep        
pcOct (OPC pc) = OR $ x + (8.33333 * (pc - x))
  where
    x = fromIntegral $ floor pc

--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Read MidiPitch where 
  readsPrec _ s = readsParsec (int >>= return . M) s
  
instance Read Hertz where 
  readsPrec _ s = readsParsec (float >>= return . Hz) s
  
instance Read OctavePC where 
  readsPrec _ s = readsParsec (float >>= return . OPC) s
    
instance Read OctaveRep where 
  readsPrec _ s = readsParsec (float >>= return . OR) s
  
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------
instance Show MidiPitch where
  showsPrec _ (M i) = shows i

instance Show Hertz where
  showsPrec _ (Hz d) = shows d
  
instance Show OctavePC where
  showsPrec _ (OPC d) = shows d

instance Show OctaveRep where
  showsPrec _ (OR d) = shows d

    
    