{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types (e.g time signature) for musical structures.
--
--------------------------------------------------------------------------------

module Neume.Datatypes
  (

  -- * Notelists
    NoteGroup(..)
  , PletTree(..)
  , plet
  , duplet
  , triplet
  , simpleNoteGroup

  -- * Meter patterns and time signatures
  , MeterPattern
  , makeMeterPattern
  , compoundMeter
  , simpleMeter
  , TimeSignature(..)
  , MetricalSpec(..)
   
  ) where

import Neume.Utils


-- | A 'NoteGroup' is a list of notes (or more properly glyphs).
-- 
-- This is the initial data structure representing musical 
-- fragments. A NoteGroup is processed by Neume (split into bars
-- and beamed according to a 'MeterPattern'), then rendered to
-- ABC or LilyPond.
--
-- To handle n-ary tuplets a NoteGroup is unfortunately somewhat 
-- more complicated than a simple (linear) list.
--
newtype NoteGroup a = NoteGroup { getNoteGroup :: [PletTree a] }
  deriving (Eq)

-- | A \PletTree\ represents an element in a 'NoteGroup'. A 
-- element may be either a single note (constructor S) or an 
-- n-ary tuplet (constructor Plet) which is recursive, so that 
-- tuplets can contain tuplets.
--
data PletTree a = S a                           -- Single \"note\"
                | Plet Int Int (NoteGroup a)
  deriving (Eq,Show)


instance Show a => Show (NoteGroup a) where
  showsPrec i (NoteGroup xs) = showsPrec i xs

instance Functor NoteGroup where
  fmap f = NoteGroup . map (fmap f) . getNoteGroup

instance Functor PletTree where
  fmap f (S a) = S (f a)
  fmap f (Plet n d ng) = Plet n d (fmap f ng)


-- | Short-hand constructor for n-ary plets.
--
plet :: Int -> Int -> [PletTree a] -> PletTree a
plet p q xs = Plet p q (NoteGroup xs) 

-- | Create a duplet - two notes in the time of three.
--
duplet :: a -> a -> PletTree a 
duplet a b = plet 2 3 [S a,S b]

-- | Create a triplet - three notes in the time of two.
--
triplet :: a -> a -> a -> PletTree a 
triplet a b c = plet 3 2 [S a,S b,S c]


-- | Convert a list of notes / glyphs (i.e no tuplets or duplets) 
-- into a 'NoteGroup'.
--
simpleNoteGroup :: [a] -> NoteGroup a
simpleNoteGroup = NoteGroup . map S


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

data TimeSignature = TimeSignature { ts_meter :: Int , ts_pulse :: Int }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Metrical specification

data MetricalSpec = MetricalSpec { 
        timeSignature :: TimeSignature,
        meterPattern  :: MeterPattern
      }
  deriving (Eq,Show)
