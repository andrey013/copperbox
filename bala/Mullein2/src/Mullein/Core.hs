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
    Tied
  , tied
  , notTied

  , MeterPattern
  , meterPattern

  , ElementP(..)
  , Element
  , GraceNoteP(..)
  , NoteAttribute(..)
  , ScNote(..)
  , Note(..)

  ) where


import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils


import Data.Ratio


--------------------------------------------------------------------------------
-- Musical representation


type Tied = Bool

  
tied    :: Tied
tied    = True

notTied :: Tied
notTied = False


-- MeterPatterns are not [Duration]...
type MeterPattern = [Rational] 



--------------------------------------------------------------------------------
-- Meter utils
           

meterPattern :: Int -> Int -> MeterPattern
meterPattern n d 
      | compoundMeter  n d  = replicate 3 $ (rational n d) / 3
      | simpleMeter n d     = replicate n $ rational 1 d
      | otherwise           = error $ err_msg
  where
    err_msg = "meterPattern - can't generate a meter pattern for a "
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
        
data GraceNoteP e = GraceNote Duration e
  deriving (Eq,Show)




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
  getDuration (GraceNotes _) = dZero
      
  setDuration d (Note _ p)      = Note d p
  setDuration d (Rest _)        = Rest d
  setDuration d (Spacer _)      = Spacer d
  setDuration d (Chord _ se)    = Chord d se
  setDuration _ (GraceNotes se) = GraceNotes se

instance Spacer (ElementP e) where
  spacer d     = Spacer d  


instance PitchMap ScNote where
  pitchMap f (ScNote p as) = ScNote (f p) as
  
  pitchMapM mf (ScNote p as) = pitchMapM mf p >>= \p' -> return $ ScNote p' as


instance PitchMap e => PitchMap (ElementP e) where
  pitchMap f (Note d e)       = Note d (pitchMap f e)
  pitchMap _ (Rest d)         = Rest d
  pitchMap _ (Spacer d)       = Spacer d
  pitchMap f (Chord d ps)     = Chord d (map (pitchMap f) ps)
  pitchMap f (GraceNotes xs)  = GraceNotes (map (pitchMap f) xs)

  pitchMapM mf (Note d e)       = pitchMapM mf e >>= return . Note d
  pitchMapM _  (Rest d)         = return $ Rest d
  pitchMapM _  (Spacer d)       = return $ Spacer d
  pitchMapM mf (Chord d ps)     = mapM (pitchMapM mf) ps >>= return . Chord d
  pitchMapM mf (GraceNotes xs)  = mapM (pitchMapM mf) xs >>= return . GraceNotes 


instance PitchMap e => PitchMap (GraceNoteP e) where
  pitchMap f (GraceNote d p) = GraceNote d (pitchMap f p)

  pitchMapM mf (GraceNote d p) = pitchMapM mf p >>= return . GraceNote d












