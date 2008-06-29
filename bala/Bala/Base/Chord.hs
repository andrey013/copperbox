

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
  Chord, Inversion(..)
  
   
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


   
type IntervalMap = Map.Map Int IntervalQuality 

  
    
                
  
  
--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

-- | doen't yet handle inversions
-- notes :: Chord -> [Pitch]
notes (Chord p i m) 
    | i /= RootPosition = error $ "inversions to do"
    | otherwise         = map (extUp p) intervals
  where
    intervals = map (\(i,a) -> interval a (intervalSize i)) (Map.toAscList m)


majorTriad :: Pitch -> Chord
majorTriad p = Chord p RootPosition $ buildMap
  [perfect_unison, major_third, perfect_fifth]

minorTriad :: Pitch -> Chord
minorTriad p = Chord p RootPosition $ buildMap
  [perfect_unison, minor_third, perfect_fifth]
  

buildMap :: [Interval] -> IntervalMap
buildMap = Map.fromAscList . map fn
  where
    fn ivl = (intervalType ivl, undefined)


-- | replace or add
roa :: Int -> IntervalQuality -> Chord -> Chord
roa idx qual (Chord p i m) = Chord p i (Map.insert idx qual m)

del :: Int -> Chord -> Chord
del idx (Chord p i m) = Chord p i (Map.delete idx m)

dim i = roa i Diminished
aug i = roa i Augmented

noRoot       = del 1
noThird      = del 3
noFifth      = del 5
noSeventh    = del 7
noNinth      = del 9
noEleventh   = del 11
noThirteenth = del 13

diminishedFitfh = dim 5
augmentedFifth = aug 5

minorSeventh = roa 7 Minor
majorSeventh = roa 7 Major
diminishedSeventh = dim 7


-- Csus2 <C D G> and Csus4 <C F G>



