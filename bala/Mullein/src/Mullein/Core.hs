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

  -- * Meter patterns and time signatures
    MeterPattern
  , makeMeterPattern
  , TimeSignature
  , MetricalSpec(..)
  
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
  , StdGlyph
  , PDGlyph

  -- * Classes
  , MakeRest(..)
  , MakeSpacer(..)
  , HasTie(..)

  -- * Builders
  , makeNote
  , makeChord
   
  ) where


import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils

import Data.Semigroup   -- package: algebra

import Text.PrettyPrint.Leijen ( Doc )

import Control.Applicative
import Data.Foldable
import Data.Traversable hiding ( mapM )





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

-------------------------------------------------------------------------------
-- Time signatures

type TimeSignature = (Int,Int)

data MetricalSpec = MetricalSpec { 
        timeSignature :: TimeSignature,
        meterPattern  :: MeterPattern
      }
  deriving (Eq,Show)


 
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
instance Semigroup (Bar e) where
  (Bar x)       `append` (Bar y)       = OverlayL [x,y]
  (Bar x)       `append` (OverlayL ys) = OverlayL $ x:ys
  (OverlayL xs) `append` (Bar y)       = OverlayL $ xs ++ [y]
  (OverlayL xs) `append` (OverlayL ys) = OverlayL $ xs ++ ys

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

type Tie = Bool

data Glyph anno pch drn = Note   anno pch drn Tie
                        | Rest   drn
                        | Spacer drn
                        | Chord  [(anno,pch)] drn Tie
                        | GraceNotes [GraceNote anno pch drn]
  deriving (Eq,Show)


data GraceNote noteinfo pch drn = GraceNote noteinfo pch drn
  deriving (Eq,Show)

type StdGlyph anno = Glyph anno Pitch Duration

-- | (P)itch (D)uration glyph - basic unannotated note format.
type PDGlyph = StdGlyph ()



--------------------------------------------------------------------------------
-- Classes

class MakeSpacer e where
  makeSpacer :: Duration -> e

class MakeSpacer e => MakeRest e where
  makeRest :: Duration -> e 



class HasTie a where
  setTied :: a -> a

-- instances
  
instance HasTie (Glyph anno pch dur) where
  setTied (Note anno p d _)   = Note anno p d True
  setTied (Chord ps d _)      = Chord ps d True
  setTied e                   = e

instance HasDuration (Glyph anno pch) where 
  getDuration (Note _ _ d _)   = d
  getDuration (Rest d)       = d
  getDuration (Spacer d)     = d
  getDuration (Chord _ d _)  = d
  getDuration (GraceNotes _) = dZero



instance MakeRest (Glyph anno pch Duration) where
  makeRest drn = Rest drn

instance MakeSpacer (Glyph anno pch Duration) where
  makeSpacer = Spacer 




--------------------------------------------------------------------------------
-- Builders


makeNote  :: Pitch -> anno -> Duration -> StdGlyph anno
makeNote p a d = Note a p d False

makeChord :: [(Pitch, anno)] -> Duration -> StdGlyph anno
makeChord pas d = Chord (map swap pas) d False






