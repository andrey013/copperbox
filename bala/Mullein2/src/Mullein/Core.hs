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

  , Glyph(..)
  , StdGlyph
  , GraceNote(..)
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


-- Glyphs are parametric on duration and /note/ - a note may hold more 
-- info than just pitch (i.e. fingering, string number). Alternatively
-- it might something that is not really pitch such as a drum name for
-- LilyPond percussion.


data Glyph drn note = Note   drn note
                    | Rest   drn
                    | Spacer drn
                    | Chord  drn [note]
                    | GraceNotes [GraceNote drn note]
                    | Tie
  deriving (Eq,Show)

type StdGlyph = Glyph Duration Pitch


-- Wrap a newtype to be distinct from pair...

data GraceNote drn pch = GraceNote drn pch
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




instance HasDuration (Glyph Duration pch) where 
  getDuration (Note d _)     = d
  getDuration (Rest d)       = d
  getDuration (Spacer d)     = d
  getDuration (Chord d _)    = d
  getDuration (GraceNotes _) = dZero
  getDuration Tie            = dZero
      
  setDuration d (Note _ p)      = Note d p
  setDuration d (Rest _)        = Rest d
  setDuration d (Spacer _)      = Spacer d
  setDuration d (Chord _ se)    = Chord d se
  setDuration _ (GraceNotes se) = GraceNotes se
  setDuration _ Tie             = Tie

instance Spacer (Glyph Duration pch) where
  spacer d     = Spacer d  


instance PitchMap ScNote where
  pitchMap f (ScNote p as) = ScNote (f p) as
  
  pitchMapM mf (ScNote p as) = pitchMapM mf p >>= \p' -> return $ ScNote p' as


instance PitchMap note => PitchMap (Glyph drn note) where
  pitchMap f (Note d e)       = Note d (pitchMap f e)
  pitchMap _ (Rest d)         = Rest d
  pitchMap _ (Spacer d)       = Spacer d
  pitchMap f (Chord d ps)     = Chord d (map (pitchMap f) ps)
  pitchMap f (GraceNotes xs)  = GraceNotes (map (pitchMap f) xs)
  pitchMap _ Tie              = Tie


  pitchMapM mf (Note d e)       = pitchMapM mf e >>= return . Note d
  pitchMapM _  (Rest d)         = return $ Rest d
  pitchMapM _  (Spacer d)       = return $ Spacer d
  pitchMapM mf (Chord d ps)     = mapM (pitchMapM mf) ps >>= return . Chord d
  pitchMapM mf (GraceNotes xs)  = mapM (pitchMapM mf) xs >>= return . GraceNotes 
  pitchMapM _  Tie              = return Tie

instance PitchMap note => PitchMap (GraceNote drn note) where
  pitchMap f (GraceNote d p) = GraceNote d (pitchMap f p)

  pitchMapM mf (GraceNote d p) = pitchMapM mf p >>= return . GraceNote d












