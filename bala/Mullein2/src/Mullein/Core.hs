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

    Tied(..)

  -- * Types

  , MeterPattern
  , meterPattern

  , Glyph(..)
  , StdGlyph
  , GraceNote(..)

  , Phrase
  , Bar(..)
  , PulseL
  , Pulse(..)

  , NoteAttribute(..)
  , ScNote(..)
  , Note(..)

  ) where


import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils


import Control.Applicative
import Data.Foldable
import Data.Traversable hiding ( mapM )
import Data.Ratio




class Tied e where
  mkTie :: e


--------------------------------------------------------------------------------
-- Musical representation



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


data Glyph note drn = Note   note drn
                    | Rest   drn
                    | Spacer drn
                    | Chord  [note] drn
                    | GraceNotes [GraceNote note drn]
                    | Tie
  deriving (Eq,Show)

type StdGlyph = Glyph Pitch Duration


data GraceNote note drn = GraceNote note drn
  deriving (Eq,Show)

type Phrase e = [Bar e]

data Bar e = Bar (PulseL e)
           | OverlayL [PulseL e] 
  deriving (Eq,Show)

type PulseL e = [Pulse e]

-- Pulse / pulsation -- a metrical division of a bar
data Pulse e = Pulse e
             | BeamedL [e]
  deriving (Eq,Show)

instance Functor Bar where
  fmap f (Bar xs)      = Bar $ fmap (fmap f) xs
  fmap f (OverlayL xs) = OverlayL $ map (fmap (fmap f)) xs

instance Functor Pulse where
  fmap f (Pulse e) = Pulse (f e) 
  fmap f (BeamedL es) = BeamedL $ map f es

instance Foldable Bar where
  foldMap f (Bar xs) = foldMap (foldMap f) xs
  foldMap f (OverlayL xs) = foldMap (foldMap (foldMap f)) xs

instance Foldable Pulse where
  foldMap f (Pulse e) = f e
  foldMap f (BeamedL es) = foldMap f es

instance Traversable Bar where
  traverse f (Bar xs) = Bar <$> traverse (traverse f) xs
  traverse f (OverlayL xs) = OverlayL <$> traverse (traverse (traverse f)) xs

instance Traversable Pulse where
  traverse f (Pulse e) = Pulse <$> f e
  traverse f (BeamedL es) = BeamedL <$> traverse f es


instance Groupoid (Bar e) where
  Bar ps       `gappend` Bar ps'       = OverlayL [ps,ps']
  Bar ps       `gappend` OverlayL pps  = OverlayL $ ps:pps
  OverlayL pps `gappend` Bar ps        = OverlayL $ pps ++ [ps]
  OverlayL pps `gappend` OverlayL pps' = OverlayL $ pps ++ pps'
  



data NoteAttribute = Fingering Int
  deriving (Eq,Show)

data ScNote = ScNote Pitch [NoteAttribute]
  deriving (Eq,Show)

instance Tied (Glyph d n) where
  mkTie = Tie

class Note a where
  type Attr a :: * 
  type Pch  a :: * 
  mkNote :: Pch a -> Attr a -> a 

instance Note ScNote where
  type Attr ScNote = [NoteAttribute]
  type Pch  ScNote = Pitch

  mkNote pch as = ScNote pch as



instance HasDuration (Glyph pch Duration) where 
  getDuration (Note _ d)     = d
  getDuration (Rest d)       = d
  getDuration (Spacer d)     = d
  getDuration (Chord _ d)    = d
  getDuration (GraceNotes _) = dZero
  getDuration Tie            = dZero


instance Spacer (Glyph pch Duration) where
  spacer d     = Spacer d  



instance PitchMap ScNote where
  pitchMap f (ScNote p as) = ScNote (f p) as
  
  pitchMapM mf (ScNote p as) = pitchMapM mf p >>= \p' -> return $ ScNote p' as


instance PitchMap note => PitchMap (Glyph note drn) where
  pitchMap f (Note e d)       = Note (pitchMap f e) d
  pitchMap _ (Rest d)         = Rest d
  pitchMap _ (Spacer d)       = Spacer d
  pitchMap f (Chord ps d)     = Chord (map (pitchMap f) ps) d
  pitchMap f (GraceNotes xs)  = GraceNotes (map (pitchMap f) xs)
  pitchMap _ Tie              = Tie


  pitchMapM mf (Note e d)       = pitchMapM mf e >>= \p -> return (Note p d)
  pitchMapM _  (Rest d)         = return $ Rest d
  pitchMapM _  (Spacer d)       = return $ Spacer d
  pitchMapM mf (Chord ps d)     = mapM (pitchMapM mf) ps >>= \ps' -> return (Chord ps' d)
  pitchMapM mf (GraceNotes xs)  = mapM (pitchMapM mf) xs >>= return . GraceNotes 
  pitchMapM _  Tie              = return Tie

instance PitchMap note => PitchMap (GraceNote note drn) where
  pitchMap f (GraceNote p d) = GraceNote (pitchMap f p) d

  pitchMapM mf (GraceNote p d) = pitchMapM mf p >>= \p' -> return (GraceNote p' d)












