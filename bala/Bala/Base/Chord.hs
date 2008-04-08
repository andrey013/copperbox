

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


module Bala.Base.Chord where

import Bala.Base.PitchRep
import Bala.Base.PitchOps
import Bala.Base.Interval
import Bala.Base.PitchClass
import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Pos

data Inversion = RootPosition | FirstInversion | SecondInversion 
               | ThirdInversion | NthInversion Int
  deriving (Eq,Read,Show)

data Chord = Chord { 
    chord_root  :: Pitch,
    inversion   :: Inversion,
    chord_elems :: Map.Map Int IntervalQuality 
  }
  deriving (Show)
   


  
    
                
  
  
--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

-- | doen't yet handle inversions
-- notes :: Chord -> [Pitch]
notes (Chord p i m) 
    | i /= RootPosition = error $ "inversions to do"
    | otherwise         = map (extUp p) intervals
  where
    intervals = map (uncurry $ flip interval') (Map.toAscList m)


majorTriad :: Pitch -> Chord
majorTriad p = Chord p RootPosition $ 
  Map.fromAscList [(1,Perfect),(3,Major),(5,Perfect)]

minorTriad :: Pitch -> Chord
minorTriad p = Chord p RootPosition $ 
  Map.fromAscList [(1,Perfect),(3,Minor),(5,Perfect)]
  

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



