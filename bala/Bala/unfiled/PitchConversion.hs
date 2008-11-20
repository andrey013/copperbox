

--------------------------------------------------------------------------------
-- |
-- Module      :  PitchConversion
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Convert between pitch representations
--
--------------------------------------------------------------------------------

module PitchConversion (
  
  -- * Alternative Pitch representations
  MidiPitch, Hertz, OctavePitchClass, OctaveFractional,
  
  -- * Constructors
  midiPitch, hertz, octavePitchClass, octaveFractional,
  
  -- * Destructors
  midiValue, hertzValue, ovePCValue, oveFrValue,
  
  
  -- * Conversion
  hzPC, hzMidi, hzOct,
  midiHz, midiOct, midiPC,
  octHz, octMidi, octPC,
  pcMidi, pcHz, pcOct
  
  ) where

import Bala.Base.Pitch
import Bala.Base.BaseExtra

import Data.Word


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- | @midi@
newtype MidiPitch = M {getMidiPitch :: Int}
  deriving (Eq)

-- | @hz@
newtype Hertz = Hz {getHertz :: Float}
  deriving (Eq)
  
-- | @pc@
newtype OctavePitchClass = OvePC {getOvePC :: Float}
  deriving (Eq)

-- | @oct@  
newtype OctaveFractional = OveFr {getOveFr :: Float}
  deriving (Eq)
  
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

hertz :: Float -> Hertz
hertz = Hz

midiPitch :: Int -> MidiPitch
midiPitch = M 

octavePitchClass :: Float -> OctavePitchClass
octavePitchClass = OvePC

octaveFractional :: Float -> OctaveFractional
octaveFractional = OveFr


midiValue :: MidiPitch -> Int
midiValue = getMidiPitch

hertzValue :: Hertz -> Float
hertzValue = getHertz

ovePCValue :: OctavePitchClass -> Float
ovePCValue = getOvePC

oveFrValue :: OctaveFractional -> Float
oveFrValue = getOveFr


-- | Convert to and from the primary pitch representation.
class EncodePitch a where 
    -- | Convert to a Pitch.
    toPitch       :: a -> Pitch 
    -- | Convert from a Pitch. 
    fromPitch     :: Pitch -> a
    
    
-- Midi middle C is 60, whereas for Pitch it is 48
instance EncodePitch MidiPitch where
  fromPitch p = M $ semitones p + 12

  toPitch m@(M i) = fromInteger $ fromIntegral (i - 12)

{-
mkMidi offst accdt octv = M $ offst + semitones accdt + octaveMidi octv
  where octaveMidi o            = (1 + o) * 12
-}


hzPC :: Hertz -> OctavePitchClass
hzPC (Hz hz) = OvePC $ y + (12.0*z) / 100.0
  where
    k :: Float
    k = 8.75 + logBase 2.0 (hz / 440.0)
    
    y = fromIntegral $ round k
    
    z = k - y

hzMidi :: Hertz -> MidiPitch
hzMidi (Hz hz) = M $ round $ (12*((log hz / log 2.0)-a)) - 3
  where
    a = log 6.875 / log 2.0
      

hzOct :: Hertz -> OctaveFractional
hzOct (Hz hz) = OveFr $ log (hz / 1.021975) / 0.69314718

midiHz :: MidiPitch -> Hertz 
midiHz (M m) = Hz $ 6.875 * (2.0 ** ((fromIntegral $ m+3) / 12)) 

midiOct :: MidiPitch -> OctaveFractional 
midiOct (M m) = OveFr $ 8.0 + (fromIntegral m -60)/12.0
  

midiPC :: MidiPitch -> OctavePitchClass
midiPC (M m) = OvePC $ ((frac*12.0) / 100.0) + fromIntegral i
  where
    i :: Int
    i = 8 +  floor ((fromIntegral m - 60.0) / 12.0)
    frac :: Float
    frac = (fromIntegral $ m - (60 +(12*(i-8)))) / 12.0
    
octHz :: OctaveFractional -> Hertz
octHz (OveFr oct) = Hz $ 1.021975 * (2.0 ** x)
  where
    x = fromIntegral $ floor oct

octMidi :: OctaveFractional -> MidiPitch
octMidi (OveFr oct) = M $ floor $ k + 0.5
  where
    k = (12.0*(oct-8.0)) + 60.0
  

octPC :: OctaveFractional -> OctavePitchClass
octPC (OveFr oct) = OvePC $ (0.12*(oct-x)) + x
  where
    x = fromIntegral $ floor oct 


        
pcMidi :: OctavePitchClass -> MidiPitch
pcMidi (OvePC pc) = M $ 60 + (12*(i-8)) + fromEnum ((100.0*(pc - (fromIntegral i)))+0.5)
  where i ::Int
        i = floor pc
    


pcHz :: OctavePitchClass -> Hertz
pcHz (OvePC pc)  = Hz $ (2.0 ** (oct + (8.333333 * (pc - oct)))) * 1.021975
  where
    oct :: Float
    oct = fromIntegral $ floor pc
  
        
pcOct :: OctavePitchClass -> OctaveFractional        
pcOct (OvePC pc) = OveFr $ x + (8.33333 * (pc - x))
  where
    x = fromIntegral $ floor pc


-- | A Word8 pitch will always be a MIDI value

instance EncodePitch Word8 where
  fromPitch p = fromIntegral $ getMidiPitch p'
    where 
      p' = fromPitch p

  toPitch i  = toPitch (M $ fromIntegral i)
  
  

    