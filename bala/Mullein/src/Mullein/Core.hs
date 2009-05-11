{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Core
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Core music representation datatypes and functions operating on them.
--
--------------------------------------------------------------------------------

module Mullein.Core where


import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils


import Data.Ratio
import Text.PrettyPrint.Leijen ( Doc )

--------------------------------------------------------------------------------
-- Musical representation


type MeterPattern = [Duration] 

type MetricalSpec = (Meter,MeterPattern)

  
data Key = Key PitchLabel Mode
  deriving (Eq,Show) 


-- ABC has /extended keys/ that contain a list of modifiers to the 
-- pitches in the original key.
data ExtKey = ExtKey Key [PitchLabel]
  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Bounded,Enum,Eq,Ord,Show) 


--------------------------------------------------------------------------------
-- Meter utils

meterFraction :: Meter -> Duration
meterFraction (TimeSig n d) = n%d
meterFraction CommonTime    = 4%4 
meterFraction CutTime       = 2%2 
           

metricalSpec :: Int -> Int -> MetricalSpec
metricalSpec n d 
      | compoundMeter  n d  = (time_sig, replicate 3 $ (rational n d) / 3)
      | simpleMeter n d     = (time_sig, replicate n (rational 1 d))
      | otherwise           = error $ err_msg
  where
    time_sig = TimeSig (fromIntegral n) (fromIntegral d)
 
    err_msg = "simpleMetricalSpec - can't generate a meter pattern for a "
           ++ "meter that is neither simple or compound."

withMeterPattern :: MeterPattern -> MetricalSpec -> MetricalSpec
withMeterPattern m (ts,_) = (ts,m) 

unitNote :: MetricalSpec -> Duration
unitNote (m,_) | meterFraction m >= 3%4 = 1%8
               | otherwise              = 1%16


-- Note compoundMeter and simpleMeter overlap

compoundMeter :: Integral a => a -> a -> Bool
compoundMeter n d = log2whole d && (n `mod` 3 == 0)
         
simpleMeter :: Integral a => a -> a -> Bool
simpleMeter _ d = log2whole d

log2whole :: Integral a => a -> Bool
log2whole = (==0) . snd . pf . logBase 2 . fromIntegral where
    pf :: Double -> (Int, Double)
    pf = properFraction


 
--------------------------------------------------------------------------------
-- Representing scores 

data PartP e  = Part [PhraseP e]
  deriving (Eq,Show)

data PhraseP e = Phrase   (MotifP e)
               | Repeated (MotifP e)
               | FSRepeat (MotifP e) (MotifP e) (MotifP e)
  deriving (Eq,Show)

data MotifP e = Motif Key Meter [BarP e]
  deriving (Eq,Show)

data BarP e = Bar     (UnisonP e)
            | Overlay (UnisonP e) [UnisonP e]
  deriving (Eq,Show)


type Tied = Bool

data UnisonP e = Unison [BracketP e] Tied
  deriving (Eq,Show)


-- bracket together beamed notes to make a pulsation
data BracketP e = Singleton (ElementP e)
                | Bracket   [ElementP e]
  deriving (Eq,Show)


-- Pitch is the typical parameter for Element syntax tree.
-- However other variations so as LilyPond percussion can be handled.
-- With LilyPond percussion each note is a drum name rather than a pitch. 

data ElementP e = Note   e     Duration
                | Rest   Duration
                | Spacer Duration
                | Chord  [e]   Duration
                | GraceNotes [GraceNoteP e]
  deriving (Eq,Show)
        
type GraceNoteP e = (e, Duration)


instance Temporal (ElementP e) where 
  duration (Note _ d)             = d
  duration (Rest d)               = d
  duration (Spacer d)             = d
  duration (Chord _ d )           = d
  duration (GraceNotes _)         = 0
    
 
  
  swapDuration d (Note p _)       = Note p d
  swapDuration d (Rest _)         = Rest d
  swapDuration d (Spacer _)       = Spacer d
  swapDuration d (Chord se _)     = Chord se d
  swapDuration _ (GraceNotes se)  = GraceNotes se

instance Spacer (ElementP e) where
  spacer d     = Spacer d  

--------------------------------------------------------------------------------
-- Note lists


type BarNum = Int
type OverlayList e = ([ElementP e], [(BarNum,[ElementP e])])



--------------------------------------------------------------------------------
-- Pretty printing scores


-- A Phantom type 
newtype P a = P { unP :: Doc }


infixl 5 +++
class Concat ctx ctx' where
  (+++)  :: P ctx -> P ctx' -> P ctx'











