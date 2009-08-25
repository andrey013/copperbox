{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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

  -- * Meter patterns
    MeterPattern
  , makeMeterPattern

  -- * Score representation
  , Phrase
  , Bar(..)
  , Pulse(..)

  , DPhrase
  , DBar
  , DOverlay

  -- ** Glyphs
  , Glyph(..)
  , GraceNote(..)
  , PDGlyph

  -- * Classes
  , MakeNote(..)
  , MakeRest(..)
   
  ) where


import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils

import Text.PrettyPrint.Leijen ( Doc )

import Control.Applicative
import Data.Foldable
import Data.Traversable hiding ( mapM )
import Data.Ratio





--------------------------------------------------------------------------------
-- Meter patterns


-- Implementation note - MeterPatterns must support arithmetic
-- so are lists of Rationals rather that the lists of Duration.


type MeterPattern = [Rational] 
     

makeMeterPattern :: Int -> Int -> MeterPattern
makeMeterPattern n d 
      | compoundMeter  n d  = replicate 3 $ (makeRational n d) / 3
      | simpleMeter n d     = replicate n $ makeRational 1 d
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
-- Score representation

-- Phrases/Bars/Pulsations

-- Note - overlay is unfortunately essential. It would be nice if 
-- overlays could be delegated to a PP combinator, it is needed in 
-- the LilyPond relative pitch / relative duration transformations.

type Phrase e = [Bar e]

data Bar e = Bar (PulseL e)
           | OverlayL [PulseL e] 
  deriving (Eq,Show)

type PulseL e = [Pulse e]

-- Pulse / pulsation -- a metrical division of a bar
data Pulse e = Pulse e
             | BeamedL [e]
  deriving (Eq,Show)

-- Functor

instance Functor Bar where
  fmap f (Bar xs)      = Bar $ fmap (fmap f) xs
  fmap f (OverlayL xs) = OverlayL $ map (fmap (fmap f)) xs

instance Functor Pulse where
  fmap f (Pulse e) = Pulse (f e) 
  fmap f (BeamedL es) = BeamedL $ map f es

-- Foldable

instance Foldable Bar where
  foldMap f (Bar xs) = foldMap (foldMap f) xs
  foldMap f (OverlayL xs) = foldMap (foldMap (foldMap f)) xs

instance Foldable Pulse where
  foldMap f (Pulse e) = f e
  foldMap f (BeamedL es) = foldMap f es

-- Traversable

instance Traversable Bar where
  traverse f (Bar xs) = Bar <$> traverse (traverse f) xs
  traverse f (OverlayL xs) = OverlayL <$> traverse (traverse (traverse f)) xs

instance Traversable Pulse where
  traverse f (Pulse e) = Pulse <$> f e
  traverse f (BeamedL es) = BeamedL <$> traverse f es

-- Groupoid 
instance Groupoid (Bar e) where
  (Bar x)       `gappend` (Bar y)       = OverlayL [x,y]
  (Bar x)       `gappend` (OverlayL ys) = OverlayL $ x:ys
  (OverlayL xs) `gappend` (Bar y)       = OverlayL $ xs ++ [y]
  (OverlayL xs) `gappend` (OverlayL ys) = OverlayL $ xs ++ ys

-------

-- It is much more flexible to treat Bars and Overlays as Docs 
-- after they have been beamed and rendered. This way we can have
-- arbitrary functions for /mixing/ overlays e.g. prefixing each 
-- overlay with stemUp or stemDown commands.
type DPhrase    = [DBar]

type DBar       = [DOverlay]

type DOverlay   = Doc

--------------------------------------------------------------------------------
-- Glyphs

-- TODO - To handle triplets or their generalization /n-plets/, 
-- glyph would need another special case.

-- | The glyph type is usefully comprehensive for representing 
-- musical /atoms/ (notes and rests). Chords and graces notes
-- turn out to be special cases that can't be represented as lists
-- of notes - they need to signal special /directives/ score 
-- output. Spacer rests are unprinted rests, they are essential
-- for polyphonic music where independent musical lines are 
-- printed on the same staff. Ties support the printing of notes 
-- with elaborate duration.
--
-- Glyphs are parametric on duration and pitch - a pitch may 
-- hold more info than just pitch (i.e. fingering, string number). 
-- A pitch does not have to be the Pitch type in Mullein.Pitch 
-- it might be e.g. a LilyPond drum pitch which is really just an 
-- enumeration of drum names.


data Glyph pch drn = Note   pch drn
                   | Rest   drn
                   | Spacer drn
                   | Chord  [pch] drn
                   | GraceNotes [GraceNote pch drn]
                   | Tie
  deriving (Eq,Show)


data GraceNote pch drn = GraceNote pch drn
  deriving (Eq,Show)

-- | (P)itch (D)uration glyph - the standard note format.
type PDGlyph = Glyph Pitch Duration



--------------------------------------------------------------------------------
-- Classess

class MakeNote e where
  makeNote :: Pitch -> Duration -> e

class MakeRest e where
  makeRest :: Duration -> e 

-- instances
  
instance HasDuration (Glyph pch) where 
  getDuration (Note _ d)     = d
  getDuration (Rest d)       = d
  getDuration (Spacer d)     = d
  getDuration (Chord _ d)    = d
  getDuration (GraceNotes _) = dZero
  getDuration Tie            = dZero


instance Spacer (Glyph pch Duration) where
  makeSpacer d     = Spacer d  



instance MakeNote PDGlyph where
  makeNote pch drn = Note pch drn

instance MakeRest PDGlyph where
  makeRest drn = Rest drn










