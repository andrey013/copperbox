{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
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

module Mullein.Core 
  ( 
  -- * Types
    MeterPattern
  , MetricalSpec
  , Key(..)
  , Mode(..)
  
  , meterFraction
  , metricalSpec
  , withMeterPattern
  , unitNote

  , ElementP(..)
  , Element
  , GraceNoteP
  , NoteAttribute(..)
  , ScNote(..)

  , Note(..)

  
  , BarNum

  , P(..)
  , Concat(..)

  ) where


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


-- Pitch is the typical parameter for Element syntax tree.
-- However other variations so as LilyPond percussion can be handled.
-- With LilyPond percussion each note is a drum name rather than a pitch. 

data ElementP e = Note   Duration e
                | Rest   Duration
                | Spacer Duration
                | Chord  Duration [e]
                | GraceNotes [GraceNoteP e]
  deriving (Eq,Show)

type Element = ElementP ScNote
        
type GraceNoteP e = (e, Duration)


data NoteAttribute = Fingering Int
  deriving (Eq,Show)

data ScNote = ScNote Pitch [NoteAttribute]
  deriving (Eq,Show)

class Note a where
  type Attr a :: * 
  type Pch  a :: * 
  mkNote :: Pch a -> Attr a -> a 

instance Note ScNote where
  type Attr ScNote = [NoteAttribute]
  type Pch  ScNote = Pitch

  mkNote pch as = ScNote pch as




instance HasDuration (ElementP e) where 
  getDuration (Note d _)     = d
  getDuration (Rest d)       = d
  getDuration (Spacer d)     = d
  getDuration (Chord d _)    = d
  getDuration (GraceNotes _) = 0
      
  setDuration d (Note _ p)      = Note d p
  setDuration d (Rest _)        = Rest d
  setDuration d (Spacer _)      = Spacer d
  setDuration d (Chord _ se)    = Chord d se
  setDuration _ (GraceNotes se) = GraceNotes se


instance Spacer (ElementP e) where
  spacer d     = Spacer d  

--------------------------------------------------------------------------------
-- Note lists


type BarNum = Int



--------------------------------------------------------------------------------
-- Pretty printing scores


-- A Phantom type 
newtype P a = P { unP :: Doc }


infixl 5 +++
class Concat ctx ctx' where
  (+++)  :: P ctx -> P ctx' -> P ctx'











