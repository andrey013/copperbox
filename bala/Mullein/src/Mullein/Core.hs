{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Core
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- TODO
--
--------------------------------------------------------------------------------

module Mullein.Core where

import Mullein.Cardinal
import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils

import Data.Ratio
import Data.Sequence ( (|>) )
import qualified Data.Sequence as S



--------------------------------------------------------------------------------
-- Note lists

-- The Element datatype - represents elements with a 'unit duration'.
-- E.g a chord has a set of pitches but the unit duration is common to all 
-- of them. 
data Element = 
      Note 
        { note_pitch          :: Pitch
        , elt_duration        :: Duration
        }                  
    | Rest  
        { elt_duration        :: Duration }
    | Spacer  
        { elt_duration        :: Duration }
    | Chord 
        { chord_elements      :: [Pitch] 
        , rhythmic_value      :: Duration
        }          
    | GraceNotes 
        { grace_elements      :: [GraceNote] }                              
    | Nplet 
        { nplet_multipier     :: Int
        , unit_duration       :: Duration
        , nplet_elements      :: [Pitch] 
        }                   
  deriving (Show) 

type GraceNote = (Pitch,Duration)

type NoteList = S.Seq Element


note :: Pitch -> Duration -> NoteList -> NoteList
note p d t = t |> (Note p d)

rest :: Duration -> NoteList -> NoteList
rest d t = t |> (Rest d)

root :: NoteList
root = S.empty


--------------------------------------------------------------------------------
-- structured /sections/.

newtype Section a = Section { getSection :: [Overlay a] }
  deriving (Show)

-- Follow the Abc style when voice overlays are grouped in whole bars.
type Overlay a         = Cardinal (Bar a)

type BeamGroup a = Cardinal a

data Bar a  = Bar [BeamGroup a] | TiedBar a [BeamGroup a]
  deriving (Show)              


instance Temporal Element where 
  duration (Note _ d)             = d
  duration (Rest d)               = d
  duration (Spacer d)             = d
  duration (Chord _ d )           = d
  duration (GraceNotes _)         = duration_zero
  duration (Nplet i d _)          = npletDuration i d
 
  
  swapDuration d (Note p _)       = Note p d
  swapDuration d (Rest _)         = Rest d
  swapDuration d (Spacer _)       = Spacer d
  swapDuration d (Chord se _)     = Chord se d
  swapDuration _ (GraceNotes se)  = GraceNotes se
  swapDuration d (Nplet i _ se)   = Nplet i ud se
    where ud = reunit d i se

        
reunit :: Duration -> Int -> [a] -> Duration
reunit tot i xs = tot * (makeDuration l i) * (makeDuration 1 l) where
                    l = length xs 
                  
                  
                  
instance Spacer Element where
  spacer d = Spacer d

                  
npletDuration :: Int -> Duration -> Duration
npletDuration len unit_d = (fromIntegral len % 1) * unit_d  

--------------------------------------------------------------------------------
-- aggregate sections

data Aggregate a = Aggregate a :>> Aggregate a
                 | Literal (Section a)
                 | Repeated (Section a)                 
                 | AltRepeat { body, end1, end2 :: Section a }
                 | KeyChange Key 


-- Do automatic coercion on snoc-ing...
class Snoc c c' where
  (|>>) :: c a -> c' a -> Aggregate a

instance Snoc Section Section where
  (|>>) a b = Literal a :>> Literal b
  
instance Snoc Aggregate Aggregate where
  (|>>) a b = a :>> b
  
instance Snoc Section Aggregate where
  (|>>) a b = Literal a :>> b
    
instance Snoc Aggregate Section where
  (|>>) a b = a :>> Literal b
  
repeated :: Section a -> Aggregate a
repeated = Repeated

keyChange :: Key -> Aggregate a
keyChange = KeyChange



--------------------------------------------------------------------------------
-- Musical representation

-- For /universality/ meter is defined according to Abc's representation.
-- LilyPond will simply generate @TimeSig@ cases.
data Meter = TimeSig Integer Integer 
           -- | CommonTime is 4/4
           | CommonTime 
           -- | CutTime is 2/2
           | CutTime
  deriving (Eq,Show)

type MeterPattern = [Duration] 

type MetricalSpec = (Meter,MeterPattern)

  
data Key = Key PitchLabel Mode [PitchLabel]
  deriving (Eq,Show) 

  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Eq,Enum,Ord,Show) 


--------------------------------------------------------------------------------
-- Meter

meterFraction :: Meter -> Duration
meterFraction (TimeSig n d) = n%d
meterFraction CommonTime    = 4%4 
meterFraction CutTime       = 2%2 
           

metricalSpec :: Integral a => a -> a -> MetricalSpec
metricalSpec n d 
      | compoundMeter  n d  = (time_sig, replicate 3 $ (rational n d) / 3)
      | simpleMeter n d     = (time_sig, replicate (fromIntegral n) (rational 1 d))
      | otherwise           = error $ err_msg
  where
    time_sig = TimeSig (fromIntegral n) (fromIntegral d)
 
    err_msg = "simpleMetricalSpec - can't generate a meter pattern for a "
           ++ "meter that is neither simple or compound."



-- Note compoundMeter and simpleMeter overlap

compoundMeter :: Integral a => a -> a -> Bool
compoundMeter n d = log2whole d && (n `mod` 3 == 0)
         
simpleMeter :: Integral a => a -> a -> Bool
simpleMeter _ d = log2whole d

log2whole :: Integral a => a -> Bool
log2whole = (==0) . snd . pf . logBase 2 . fromIntegral where
    pf :: Double -> (Int, Double)
    pf = properFraction
        
           