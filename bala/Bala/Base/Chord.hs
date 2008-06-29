

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Chord
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Chord representations
--
--------------------------------------------------------------------------------


module Bala.Base.Chord (
  -- * Datatypes (Chord is opaque)
  Chord, Inversion(..),
  
  -- * operations
  extractNotes, 
 
  majorTriad, minorTriad, diminishedFifth, augmentedFifth, 

  minorSeventh, majorSeventh, diminishedSeventh
 
  ) where

import Bala.Base.Pitch
import Bala.Base.Interval
import Bala.Base.PitchClass
import Bala.Base.BaseExtra


import qualified Data.Map as Map

data Chord = Chord { 
    chord_root  :: Pitch,
    inversion   :: Inversion,
    chord_elems :: IntervalMap
  }
  deriving (Show)

data Inversion = RootPosition | FirstInversion | SecondInversion 
               | ThirdInversion | NthInversion Int
  deriving (Eq,Read,Show)


   
type IntervalMap = Map.Map Int Interval 

  
    
                
  
  
--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

-- | doen't yet handle inversions
extractNotes :: Chord -> [Pitch]
extractNotes (Chord p i m) 
    | i /= RootPosition = error $ "inversions to do"
    | otherwise         = map (extUp p) intervals
  where
    intervals = map snd (Map.toAscList m)


majorTriad :: Pitch -> Chord
majorTriad p = Chord p RootPosition $ buildMap
  [perfect_unison, major_third, perfect_fifth]

minorTriad :: Pitch -> Chord
minorTriad p = Chord p RootPosition $ buildMap
  [perfect_unison, minor_third, perfect_fifth]
  

buildMap :: [Interval] -> IntervalMap
buildMap = Map.fromAscList . map fn
  where
    fn ivl = (intervalType ivl, ivl)


-- | replace or add
roa :: Int -> Interval -> Chord -> Chord
roa idx ivl (Chord p i m) = Chord p i (Map.insert idx ivl m)

del :: Int -> Chord -> Chord
del idx (Chord p i m) = Chord p i (Map.delete idx m)

dim i = roa i $ interval Diminished (intervalSize i) 
aug i = roa i $ interval Augmented (intervalSize i)

noRoot       = del 1
noThird      = del 3
noFifth      = del 5
noSeventh    = del 7
noNinth      = del 9
noEleventh   = del 11
noThirteenth = del 13

diminishedFifth = dim 5
augmentedFifth = aug 5

minorSeventh = roa 7 minor_seventh
majorSeventh = roa 7 major_seventh
diminishedSeventh = dim 7


-- Csus2 <C D G> and Csus4 <C F G>



